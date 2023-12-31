CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-07T19:16:40Z AOML 3.0 creation; 2016-06-01T00:08:25Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150707191640  20160531170825  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               xA   AO  4055_7112_120                   2C  D   APEX                            5374                            041511                          846 @�^'Tb�1   @�^'�?�@:�=p��
�dN�t�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    xA   A   A   @�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D� D�<�D��3D��fD�fD�@ D���D���D��3D�\�D�� D�� D�fD�@ D�\�D๚D��D�,�D�p D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@ʏ\AG�A&�HAEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�By�RB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=D D �DD�DD�DD�DD�DD�DD��D�D�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�Dt�Dy��D��D�G\D���D���D� �D�J�D��)D��\D���D�g\D���D�ڏD��D�J�D�g\D��)D��)D�7\D�z�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�(�A��A�JA��/A�n�A�r�AƅA�/Aš�A�{A�ȴA���A²-A�?}A���A�t�A���A�G�A�|�A�XA���A�p�A��A���A�5?A��A��HA�ȴA�
=A��+A��A�bA�JA��mA�K�A�
=A�S�A��9A�K�A�oA��;A��^A�XA���A��A�`BA�VA��A�A��hA�^5A�(�A��A�JA�
=A���A���A�r�A��-A�ZA��A�7LA��A�ȴA��`A��A�E�A��RA�p�A�-A��A�z�A�$�A�S�A�XA��7A�ZA��A�JA�C�A�VA��mA�{A�|�A�S�A���A�n�A�{A�ĜA�ffA���A��^A�dZA�XA��
A�~�A�VA�/A�  A�hsA�%A��A��A�r�A�K�A��A��hA��!A���A~z�A}�wA}VAz�AyoAy%AxȴAwAv�uAuG�At�uAr��Aq
=ApA�Ao�AnjAnJAm��Al�Ak�Ai�
AhE�Ag�Af��Af�Ae�-Ae
=Acl�AbĜAb��AaXA`�A`1A^��A^ �A]��A]�7A]C�A\�\A\{A[�FA[p�AZz�AX�uAWt�AVĜAVbNAT�DASG�ARI�AQoAP��APAO�-AOhsANĜAM�AM;dAL��AK�PAKoAJ�HAJĜAJ�uAJZAH��AG�mAF�+AD�jAC/AB(�A@$�A<�A:�uA9��A9;dA81'A7t�A6�jA5�A4�A4v�A3�TA2�A1��A0E�A/dZA.ffA-�#A-&�A,��A,9XA+�#A+�hA*~�A)oA(�A'S�A&{A$E�A#�mA#%A!�mA!/A 9XA�AC�A�AAoA��A~�A(�A��A�AbNA�-A/A-AE�A�A�A��A�jA�+A�\A��A�!A�+AE�A��A��AVA��Al�A"�A��Az�AA�A��A$�AƨA\)A��AbNAp�A
$�A	�A��AZA-A  Ap�A��A��A �A;dA�FA �A bN@�
=@��-@�9X@�n�@�&�@��`@���@�(�@��@�M�@��y@�/@�ƨ@�$�@�A�@땁@���@�P@�O�@�%@�!@�Ĝ@��m@�ff@�Ĝ@�~�@١�@�%@ׅ@��;@��H@�7L@�1@��
@�"�@�J@��/@�\)@�~�@�@�/@�  @Ǿw@Ǯ@ǍP@�S�@�K�@�"�@��H@őh@�b@�\)@�"�@§�@�~�@�E�@���@�@��^@���@� �@�o@��@��@�  @�o@�E�@��@�O�@�ƨ@�\)@�n�@���@�bN@��@�~�@���@��j@��@�@���@��;@�@�v�@�@��-@�G�@��j@�j@��w@�n�@���@�7L@��@���@��H@��!@���@��+@�v�@�E�@��-@��@���@�(�@���@��@�{@��^@��@���@�@��@���@�I�@���@�"�@�@��R@��h@�X@�O�@�7L@��@��`@�r�@��
@�S�@���@�?}@��u@�I�@�1@�ƨ@�dZ@�
=@��+@�E�@�=q@�=q@�5?@��@��#@��-@�%@�1@��;@��@�t�@��H@��!@��!@���@���@��+@�ff@��@���@���@�@��^@��-@��@�X@�/@�b@��w@���@�;d@��@��+@�ff@�E�@�5?@�-@�$�@�J@���@��T@��-@�G�@���@���@��u@�r�@�Z@�Q�@�A�@��@���@�\)@�+@�C�@�"�@��y@��+@�$�@���@���@�1'@�w@�P@\)@+@~��@~�@~v�@}O�@{S�@{@z�!@z�@y�^@x��@x��@wK�@v{@v{@u��@qx�@fv�@\��@X1'@QX@J=q@FV@@bN@:M�@4�@.$�@'�w@#o@ȴ@-@�y@t�@V@
-@��@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�(�A��A�JA��/A�n�A�r�AƅA�/Aš�A�{A�ȴA���A²-A�?}A���A�t�A���A�G�A�|�A�XA���A�p�A��A���A�5?A��A��HA�ȴA�
=A��+A��A�bA�JA��mA�K�A�
=A�S�A��9A�K�A�oA��;A��^A�XA���A��A�`BA�VA��A�A��hA�^5A�(�A��A�JA�
=A���A���A�r�A��-A�ZA��A�7LA��A�ȴA��`A��A�E�A��RA�p�A�-A��A�z�A�$�A�S�A�XA��7A�ZA��A�JA�C�A�VA��mA�{A�|�A�S�A���A�n�A�{A�ĜA�ffA���A��^A�dZA�XA��
A�~�A�VA�/A�  A�hsA�%A��A��A�r�A�K�A��A��hA��!A���A~z�A}�wA}VAz�AyoAy%AxȴAwAv�uAuG�At�uAr��Aq
=ApA�Ao�AnjAnJAm��Al�Ak�Ai�
AhE�Ag�Af��Af�Ae�-Ae
=Acl�AbĜAb��AaXA`�A`1A^��A^ �A]��A]�7A]C�A\�\A\{A[�FA[p�AZz�AX�uAWt�AVĜAVbNAT�DASG�ARI�AQoAP��APAO�-AOhsANĜAM�AM;dAL��AK�PAKoAJ�HAJĜAJ�uAJZAH��AG�mAF�+AD�jAC/AB(�A@$�A<�A:�uA9��A9;dA81'A7t�A6�jA5�A4�A4v�A3�TA2�A1��A0E�A/dZA.ffA-�#A-&�A,��A,9XA+�#A+�hA*~�A)oA(�A'S�A&{A$E�A#�mA#%A!�mA!/A 9XA�AC�A�AAoA��A~�A(�A��A�AbNA�-A/A-AE�A�A�A��A�jA�+A�\A��A�!A�+AE�A��A��AVA��Al�A"�A��Az�AA�A��A$�AƨA\)A��AbNAp�A
$�A	�A��AZA-A  Ap�A��A��A �A;dA�FA �A bN@�
=@��-@�9X@�n�@�&�@��`@���@�(�@��@�M�@��y@�/@�ƨ@�$�@�A�@땁@���@�P@�O�@�%@�!@�Ĝ@��m@�ff@�Ĝ@�~�@١�@�%@ׅ@��;@��H@�7L@�1@��
@�"�@�J@��/@�\)@�~�@�@�/@�  @Ǿw@Ǯ@ǍP@�S�@�K�@�"�@��H@őh@�b@�\)@�"�@§�@�~�@�E�@���@�@��^@���@� �@�o@��@��@�  @�o@�E�@��@�O�@�ƨ@�\)@�n�@���@�bN@��@�~�@���@��j@��@�@���@��;@�@�v�@�@��-@�G�@��j@�j@��w@�n�@���@�7L@��@���@��H@��!@���@��+@�v�@�E�@��-@��@���@�(�@���@��@�{@��^@��@���@�@��@���@�I�@���@�"�@�@��R@��h@�X@�O�@�7L@��@��`@�r�@��
@�S�@���@�?}@��u@�I�@�1@�ƨ@�dZ@�
=@��+@�E�@�=q@�=q@�5?@��@��#@��-@�%@�1@��;@��@�t�@��H@��!@��!@���@���@��+@�ff@��@���@���@�@��^@��-@��@�X@�/@�b@��w@���@�;d@��@��+@�ff@�E�@�5?@�-@�$�@�J@���@��T@��-@�G�@���@���@��u@�r�@�Z@�Q�@�A�@��@���@�\)@�+@�C�@�"�@��y@��+@�$�@���@���@�1'@�w@�P@\)@+@~��@~�@~v�@}O�@{S�@{@z�!@z�@y�^@x��@x��@wK�@v{@v{@u��@qx�@fv�@\��@X1'@QX@J=q@FV@@bN@:M�@4�@.$�@'�w@#o@ȴ@-@�y@t�@V@
-@��@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�mB�fB�`B�ZB�BB�
B��B��B��B��BŢB�wB�^B�RB�FB�!B��B��B�hB�JB�PB�\B�PB�Bz�Bu�Bn�BiyBffBaHB`BBaHBaHB`BB^5BW
BR�BQ�BQ�BQ�BP�BO�BM�BI�BG�BE�BA�B@�B=qB:^B8RB5?B49B33B2-B0!B,B%�B�BuB
=B  B��B��B�TB��BȴB�}B�^B�?B�!B��B��B�VBz�BjBVBH�B>wB0!B �B�BDBB��B��B�B�yB�TB�/B��B��BƨB�-B��B��B��B�{B�PB{�BL�B/B!�B�BoB
=B  B
�B
ǮB
�'B
�B
��B
��B
�VB
�PB
�JB
�B
{�B
p�B
hsB
XB
G�B
?}B
6FB
1'B
-B
(�B
�B
uB
JB
B	��B	��B	��B	��B	�B	�fB	�HB	�;B	�
B	��B	��B	B	�wB	�jB	�^B	�RB	�3B	�!B	�B	��B	��B	��B	�bB	�DB	�+B	}�B	v�B	q�B	l�B	iyB	ffB	e`B	bNB	_;B	\)B	YB	VB	S�B	R�B	R�B	Q�B	P�B	M�B	H�B	@�B	6FB	(�B	�B	oB	B��B�B�B�B�fB�TB�BB�/B�#B�B�B��BȴBB�}B�jB�^B�RB�RB�LB�FB�9B�-B�B�B��B��B��B��B�hB�JB�7B�%B�B�B� B|�B{�Bz�By�Bw�Bu�Bs�Br�Bq�Bo�Bl�BffBbNB`BB_;B`BBcTBdZBffBgmBiyBiyBgmBdZBcTBcTBcTBbNBaHB_;B]/BZBS�BO�BM�BK�BI�BH�BD�BB�BB�BB�BC�BC�BC�B>wB9XB2-B0!B.B,B)�B'�B&�B$�B#�B"�B"�B"�B!�B �B!�B �B�B�B!�B �B�B�B�B�B�B�B�B�B�B�B{B�B�B�BuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B"�B#�B#�B#�B$�B#�B"�B$�B%�B&�B)�B,B.B.B/B/B49B49B49B:^B;dBA�BB�BE�BG�BJ�BL�BO�BXB[#B]/B^5B_;BaHBbNBcTBe`BiyBl�Bn�Bq�Bu�By�By�Bz�Bz�Bz�Bz�B}�B� B�B�B�B�7B�JB�JB�oB�uB��B��B��B��B��B��B��B��B�!B�-B�3B�9B�FB�RB�dB�qB��B��BɺB��B��B��B��B�B�B�/B�BB�HB�HB�HB�NB�ZB�`B�yB�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B	%B	+B	DB	\B	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	#�B	$�B	$�B	%�B	%�B	(�B	)�B	+B	,B	.B	.B	/B	/B	/B	33B	7LB	9XB	:^B	:^B	;dB	=qB	=qB	=qB	>wB	B�B	C�B	C�B	E�B	F�B	H�B	I�B	M�B	R�B	S�B	W
B	o�B	��B	�dB	�BB	��B
hB
�B
#�B
.B
7LB
?}B
G�B
M�B
R�B
YB
]/B
bNB
hsB
m�B
t�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�XB�QB�LB�IB�/B��B��B��B��BʫBōB�aB�JB�<B�1B�
B��B�jB�RB�1B�7B�GB�8B��Bz�Bu�Bn�Bi^BfKBa/B`(Ba.Ba-B`)B^BV�BR�BQ�BQ�BQ�BP�BO�BM�BI�BG�BE�BAkB@hB=TB:?B8:B5B4B3B2B0B+�B%�B�BWB
 B��B��B��B�6B��BșB�]B�?B�!B�B��B��B�9Bz�BjaBU�BH�B>ZB/�B �B|B#B�B��B��B�xB�WB�2B�B��BκBƆB�B��B��B�zB�[B�/B{�BL�B.�B!�BoBOB
B
��B
�bB
ǎB
�B
��B
��B
�~B
�9B
�4B
�,B
�B
{�B
p�B
hUB
W�B
G�B
?aB
6,B
1
B
,�B
(�B
�B
XB
1B
B	��B	��B	��B	��B	�B	�KB	�.B	�"B	��B	��B	˭B	�xB	�]B	�PB	�DB	�9B	�B	�	B	��B	��B	��B	�oB	�KB	�,B	�B	}�B	v�B	q�B	ltB	icB	fMB	eJB	b5B	_%B	\B	YB	U�B	S�B	R�B	R�B	Q�B	P�B	M�B	H�B	@oB	60B	(�B	�B	[B	�B��B�B�}B�nB�TB�DB�0B�B�B�B��B��BȢB�|B�mB�XB�MB�AB�?B�:B�5B�&B�B�B��B��B��B��B�oB�YB�9B�(B�B�
B�B�B|�B{�Bz�By�Bw�Bu�Bs�Br�Bq�Bo�Bl{BfWBb;B`5B_.B`2BcDBdIBfWBg_BiiBijBg[BdHBcCBcGBcCBb<Ba8B_-B]BZBS�BO�BM�BK�BI�BH�BD�BB�BB�BBBC�BC�BC�B>fB9/B2B0B.B+�B)�B'�B&�B$�B#�B"�B"�B"�B!�B �B!�B �B�B�B!�B �B�B�B�B�B�BuB�BqB�BqBlBWBeBxBeBqBWB\BhB�B�B�B�B�B�B|B�B�B�B�B�B�B�B�B�B�B �B!�B!�B"�B#�B#�B#�B$�B#�B"�B$�B%�B&�B)�B+�B.B. B/
B/	B4)B4(B4'B:LB;SBAvBB|BE�BG�BJ�BL�BO�BW�B[B]B^!B_%Ba2Bb9Bc@BeMBicBluBn�Bq�Bu�By�By�Bz�Bz�Bz�Bz�B}�B�B��B�B�B�!B�6B�1B�VB�]B�vB��B��B��B��B��B��B��B�B�B�B� B�-B�6B�JB�VB�gB�qBɡB͸BξB��B��B��B��B�B�(B�*B�,B�+B�4B�?B�FB�\B��B�B�B�B��B��B��B��B��B��B��B��B��B��B	 �B	 �B	 �B	�B	B	B	%B	AB	PB	_B	eB	oB	rB	}B	~B	~B	~B	�B	�B	�B	�B	�B	�B	!�B	!�B	#�B	$�B	$�B	%�B	%�B	(�B	)�B	*�B	+�B	-�B	-�B	.�B	.�B	.�B	3B	7-B	98B	:?B	:@B	;FB	=OB	=TB	=QB	>VB	BpB	CtB	CuB	E�B	F�B	H�B	I�B	M�B	R�B	S�B	V�B	o~B	�}B	�AB	�B	��B
BB
tB
#�B
-�B
7$B
?WB
G�B
M�B
R�B
X�B
]B
b'B
hLB
mkB
t�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.33 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708252016053117082520160531170825  AO  ARCAADJP                                                                    20150707191640    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150707191640  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150707191640  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170825  IP                  G�O�G�O�G�O�                
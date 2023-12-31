CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:48Z AOML 3.0 creation; 2016-06-01T00:08:19Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230848  20160531170819  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               SA   AO  4055_7112_083                   2C  D   APEX                            5374                            041511                          846 @��t2��1   @��t�)��@;;dZ��d�C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    SA   A   A   @�ff@�  A   A   A@  A`  A���A���A�33A�33A�33A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�fD�fD�� D��fD��fD�3D�9�D�y�D���D�fD�L�D�s3D�ٚD��D�33DږfD�ٚD�fD�L�D�l�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@ʏ\AG�A%G�AEG�AeG�A�p�A�p�A��
A��
A��
Aң�A��A��BQ�B	Q�BQ�BQ�B �B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B�u�B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD��DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DtۅDy��D��D���D���D���D��D�D)D��)D��\D��D�W\D�}�D��)D�\D�=�Dڠ�D��)D��D�W\D�w\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�p�A�ZA�S�A�I�A�&�A�bA��/A�C�A���A���A�+A�-A�;dA��A���A�G�A��#A���A�n�A��-A�VA���A�  A���A�I�A��A���A�C�A��A���A�S�A��-A�A�A�VA��A��7A�ffA��uA�bA��wA�x�A�;dA��;A�Q�A���A�n�A�VA�ĜA��A�p�A�oA��#A���A��DA�$�A�ƨA���A�hsA�VA�\)A�9XA�
=A�ĜA���A�l�A�^5A�33A��
A��\A�VA�/A�A�A�JA�ƨA���A�z�A�r�A�dZA�Q�A�S�A�O�A�&�A�VA��;A�ĜA��!A���A���A�ffA��A��
A��-A��7A�XA�1'A��A�A���A�r�A�S�A�5?A�
=A��`A���A�ĜA��\A�p�A�7LA�A��A�ȴA�?}A�v�A��TA��A��hA�7LA�"�A�A�bNA��A�t�A�A��uA�jA� �A�=qA�A�AVA|v�AzM�Ax�Au�^At  Ar^5Ao�wAmoAjv�Af�9Ad��Ab�Aa�FA_�#A^-A]x�A\��A[�A[A[oAZM�AY�TAY|�AX�AW&�AVM�ATȴAS
=AQ�AQ�AO�AN  AMXAM;dAM+AL��AL�\AJ�AG��AE|�AD�AC/A?t�A>{A=��A=��A=dZA;\)A9G�A9&�A8��A7��A7C�A6ȴA5`BA3�A2�`A2{A1�hA0�A/
=A-p�A,ffA+oA)�#A(�A'hsA%��A%t�A$(�A"1'A!�7A!K�A!?}A!?}A -A��Av�AZAM�A=qAbA�FAhsA�An�AVA^5A;dA��A~�AƨA"�A��AƨA��A��AdZA/A�AVA%A�AƨA�\Al�A
��A	33AM�AG�AI�A�-A�RAbNAA�A�#A�A9XA?}A ��A 5?@��/@�7@�{@웦@�$�@�1'@��@�@�9@�C�@�o@�@�x�@ۥ�@�l�@�+@�V@ٲ-@�G�@���@�ff@�p�@�V@Ԭ@�(�@�ƨ@ӥ�@ӍP@�;d@���@�J@с@���@���@�z�@���@�n�@�Ĝ@˅@�~�@��@�hs@���@�Z@�|�@��@�J@��`@�z�@��@�l�@��@��T@���@��@��u@�  @�|�@��j@�(�@�;d@���@�J@�@��-@�X@��@�S�@�~�@��@�&�@��`@���@��9@�j@��
@�ȴ@�ff@��@�X@��u@�1@��@��@��@���@�C�@�n�@��@��@�\)@��\@�^5@�5?@��T@��`@�bN@��
@�ȴ@�-@�@�Ĝ@��@�\)@�"�@�o@��@�ȴ@�n�@�?}@���@�bN@�1@���@��@�~�@�M�@�$�@��^@�O�@�&�@���@�bN@�(�@��m@�5?@��h@�/@��@��
@��@�K�@�ȴ@��@�@��-@�O�@���@��@��`@��/@���@��@�|�@���@�^5@�=q@��@���@��^@��@��u@�r�@��;@�dZ@���@�5?@�@���@���@���@���@��@��T@��#@���@�@���@���@�p�@�G�@���@���@��D@�bN@�I�@��
@���@���@���@���@�C�@���@���@�5?@���@�G�@��@��@��/@�Ĝ@���@�z�@�r�@�j@�Z@�A�@���@��@�K�@��@�^5@��@���@��w@�\)@��@��@��@���@�v�@�=q@��T@��-@��h@�hs@�7L@�V@���@�Ĝ@���@��@�Z@�b@��@�@��@|�@~��@}�-@}�h@}�@}`B@}/@|�@z�H@w��@o\)@h�u@_�@Xb@Q�^@Kt�@D�j@=p�@41@,�D@(A�@$��@!�^@��@r�@1@�@z�@	G�@K�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�p�A�p�A�ZA�S�A�I�A�&�A�bA��/A�C�A���A���A�+A�-A�;dA��A���A�G�A��#A���A�n�A��-A�VA���A�  A���A�I�A��A���A�C�A��A���A�S�A��-A�A�A�VA��A��7A�ffA��uA�bA��wA�x�A�;dA��;A�Q�A���A�n�A�VA�ĜA��A�p�A�oA��#A���A��DA�$�A�ƨA���A�hsA�VA�\)A�9XA�
=A�ĜA���A�l�A�^5A�33A��
A��\A�VA�/A�A�A�JA�ƨA���A�z�A�r�A�dZA�Q�A�S�A�O�A�&�A�VA��;A�ĜA��!A���A���A�ffA��A��
A��-A��7A�XA�1'A��A�A���A�r�A�S�A�5?A�
=A��`A���A�ĜA��\A�p�A�7LA�A��A�ȴA�?}A�v�A��TA��A��hA�7LA�"�A�A�bNA��A�t�A�A��uA�jA� �A�=qA�A�AVA|v�AzM�Ax�Au�^At  Ar^5Ao�wAmoAjv�Af�9Ad��Ab�Aa�FA_�#A^-A]x�A\��A[�A[A[oAZM�AY�TAY|�AX�AW&�AVM�ATȴAS
=AQ�AQ�AO�AN  AMXAM;dAM+AL��AL�\AJ�AG��AE|�AD�AC/A?t�A>{A=��A=��A=dZA;\)A9G�A9&�A8��A7��A7C�A6ȴA5`BA3�A2�`A2{A1�hA0�A/
=A-p�A,ffA+oA)�#A(�A'hsA%��A%t�A$(�A"1'A!�7A!K�A!?}A!?}A -A��Av�AZAM�A=qAbA�FAhsA�An�AVA^5A;dA��A~�AƨA"�A��AƨA��A��AdZA/A�AVA%A�AƨA�\Al�A
��A	33AM�AG�AI�A�-A�RAbNAA�A�#A�A9XA?}A ��A 5?@��/@�7@�{@웦@�$�@�1'@��@�@�9@�C�@�o@�@�x�@ۥ�@�l�@�+@�V@ٲ-@�G�@���@�ff@�p�@�V@Ԭ@�(�@�ƨ@ӥ�@ӍP@�;d@���@�J@с@���@���@�z�@���@�n�@�Ĝ@˅@�~�@��@�hs@���@�Z@�|�@��@�J@��`@�z�@��@�l�@��@��T@���@��@��u@�  @�|�@��j@�(�@�;d@���@�J@�@��-@�X@��@�S�@�~�@��@�&�@��`@���@��9@�j@��
@�ȴ@�ff@��@�X@��u@�1@��@��@��@���@�C�@�n�@��@��@�\)@��\@�^5@�5?@��T@��`@�bN@��
@�ȴ@�-@�@�Ĝ@��@�\)@�"�@�o@��@�ȴ@�n�@�?}@���@�bN@�1@���@��@�~�@�M�@�$�@��^@�O�@�&�@���@�bN@�(�@��m@�5?@��h@�/@��@��
@��@�K�@�ȴ@��@�@��-@�O�@���@��@��`@��/@���@��@�|�@���@�^5@�=q@��@���@��^@��@��u@�r�@��;@�dZ@���@�5?@�@���@���@���@���@��@��T@��#@���@�@���@���@�p�@�G�@���@���@��D@�bN@�I�@��
@���@���@���@���@�C�@���@���@�5?@���@�G�@��@��@��/@�Ĝ@���@�z�@�r�@�j@�Z@�A�@���@��@�K�@��@�^5@��@���@��w@�\)@��@��@��@���@�v�@�=q@��T@��-@��h@�hs@�7L@�V@���@�Ĝ@���@��@�Z@�b@��@�@��@|�@~��@}�-@}�h@}�@}`B@}/@|�@z�H@w��@o\)@h�u@_�@Xb@Q�^@Kt�@D�j@=p�@41@,�D@(A�@$��@!�^@��@r�@1@�@z�@	G�@K�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB  B  B��B��B��B��B��B��B�B�/B�^B��B�DB}�Bu�Bo�BcTBW
BH�B<jB6FB1'B-B'�B$�B"�B�B{B\BPB
=B%BB��B��B��B��B�B�B�B�yB�`B�NB�5B�B��BɺBŢB��B��B�wB�^B�LB�?B�-B�B��B��B��B��B��B��B��B��B��B��B�{B�hB�DB�%B�B� B|�B�B�7B�B�B� B~�B}�B|�B}�B}�B{�Bz�Bx�Bw�Bv�Bu�Bs�Bo�BjBdZBaHB^5B[#BXBW
BVBS�BP�BM�BK�BI�BF�BD�BB�B=qB:^B49B/B,B'�B�B%B��B�/B��BǮB�B��B��Bq�B?}B6FB/B(�B
��B
��B
��B
x�B
`BB
L�B
8RB
$�B
{B
B	�B	�B	��B	��B	��B	�=B	�B	x�B	o�B	jB	ffB	bNB	aHB	]/B	YB	VB	R�B	M�B	D�B	>wB	6FB	-B	'�B	#�B	�B	�B	�B	�B	�B	�B	hB	1B	  B��B��B�B�B�B�B�yB�fB�5B�B�B�B��B��B��BɺBĜB��B�wB�jB�RB�3B�B�B��B��B��B��B�uB�hB�JB�1B�%B�B�B�B� B|�B{�B{�Bz�Bz�By�Bw�Bv�Bt�Bo�BgmB_;B\)B[#BYBW
BT�BR�BO�BL�BJ�BJ�BJ�BI�BI�BH�BF�BC�B@�B=qB9XB7LB49B1'B0!B.B-B,B+B+B)�B&�B$�B#�B!�B�B{BbB\BVBJBJBDB
=B
=B
=B	7B%B	7B
=B
=B	7B	7B	7B1B	7B
=B
=BDBDBDBJBJBDBDBDBJBJBPBJBDBDBJBVB\BbBhBhBhBhBuBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B#�B#�B#�B'�B)�B,B.B1'B2-B33B49B7LB8RB=qB=qB?}BA�BD�BH�BI�BI�BI�BI�BM�BQ�BS�BS�B`BBe`BffBffBgmBl�Bn�Bp�Bt�Bw�Bx�B{�B~�B�B�B�B�%B�%B�+B�VB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�9B�FB�XB��BÖBŢBȴB��B��B��B��B��B��B��B��B�
B�B�;B�ZB�`B�`B�fB�mB�mB�B�B�B��B��B��B	B	%B	%B	%B	%B	%B	%B	+B	+B	+B	1B	1B		7B	
=B	DB	\B	bB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	%�B	'�B	-B	0!B	2-B	33B	33B	49B	5?B	6FB	6FB	7LB	7LB	7LB	:^B	<jB	>wB	A�B	G�B	N�B	P�B	T�B	XB	YB	ZB	[#B	\)B	]/B	^5B	aHB	bNB	cTB	dZB	ffB	gmB	hsB	iyB	iyB	jB	k�B	n�B	o�B	o�B	p�B	p�B	q�B	q�B	r�B	r�B	r�B	r�B	t�B	|�B	�B	��B	�wB	�B	�B	��B
JB
�B
&�B
5?B
=qB
D�B
J�B
N�B
Q�B
YB
`BB
dZB
jB
n�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B�B�B�GB��B�,B}�Bu�Bo�Bc;BV�BH�B<PB6+B1B,�B'�B$�B"�BdB^BBB4B
B
B�B��B��B��B��B�B�tB�eB�[B�CB�0B�B��B͵BɝBńB�hB�hB�ZB�DB�/B�"B�B��B��B��B��B��B��B��B��B��B�|B�`B�[B�KB�&B�B��B�B|�B��B�B��B��B�B~�B}�B|�B}�B}�B{�Bz�Bx�Bw�Bv�Bu�Bs�Bo|BjbBd;Ba)B^B[BW�BV�BU�BS�BP�BM�BK�BI�BF�BD|BBnB=QB:@B4B.�B+�B'�BzBB��B�B��BǏB��B��B�aBq�B?aB6$B.�B(�B
��B
�jB
�|B
x�B
`&B
L�B
88B
$�B
^B
B	�wB	��B	�lB	��B	�}B	�(B	��B	x�B	o�B	jhB	fPB	b7B	a3B	]B	YB	U�B	R�B	M�B	D�B	>aB	60B	,�B	'�B	#�B	�B	�B	xB	{B	vB	mB	UB	B��B��B��B�B�~B�rB�lB�iB�SB�#B�B��B��B��B��B��BɥBčB�xB�dB�VB�@B�"B�B��B��B��B��B�}B�dB�ZB�9B� B�B�B�B�B�B|�B{�B{�Bz�Bz�By�Bw�Bv�Bt�Bo�Bg^B_,B\B[BYBV�BT�BR�BO�BL�BJ�BJ�BJ�BI�BI�BH�BF�BC�B@wB=cB9/B7=B4-B0�B/�B.B- B+�B*�B*�B)�B&�B$�B#�B!�B}BlB8B3B+B!B"BB
B
B
B	B�B	B
B
B	B	B	'BB	*B
B
0BB5B5B B!B4BB2B B;B(B BB7B B+BHB5B?B?B?BWBdBcBPB]B^B^B}B�B�B�B�BxB�B�B�B�B �B!�B"�B#�B#�B#�B'�B)�B+�B.B1B2B3!B4'B7:B8@B=]B=`B?mBArBD�BH�BI�BI�BI�BI�BM�BQ�BS�BS�B`.BeMBfQBfMBgXBluBn�Bp�Bt�Bw�Bx�B{�B~�B�B�B�B�B�B�B�?B�QB�XB�\B�cB�qB�zB�}B�zB��B��B��B��B��B��B��B�B�B�.B�>B�hB�~BňBșB͹B��B��B��B��B��B��B��B��B�B�"B�@B�BB�CB�LB�QB�SB�jB�B��B��B��B��B	�B	B	B	B	B	B	B	B	B	B	B	B		B	
 B	(B	@B	FB	VB	bB	dB	�B	�B	�B	�B	�B	 �B	$�B	%�B	'�B	,�B	0B	2B	3B	3B	4B	5!B	6&B	6&B	7-B	7/B	7-B	:@B	<IB	>UB	AjB	G�B	N�B	P�B	T�B	W�B	X�B	Y�B	[B	\B	]B	^B	a)B	b.B	c5B	d:B	fDB	gNB	hUB	iWB	iZB	j]B	kgB	nwB	o|B	oB	p�B	p�B	q�B	q�B	r�B	r�B	r�B	r�B	t�B	|�B	��B	��B	�TB	��B	�zB	��B
%B
{B
&�B
5B
=KB
DwB
J�B
N�B
Q�B
X�B
`B
d4B
jWB
npB
p}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.33 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708192016053117081920160531170819  AO  ARCAADJP                                                                    20140721230848    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230848  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230848  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170819  IP                  G�O�G�O�G�O�                
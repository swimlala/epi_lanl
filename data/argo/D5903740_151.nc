CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-20T19:15:25Z AOML 3.0 creation; 2016-06-01T00:08:30Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160520191525  20160826101911  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_151                   2C  D   APEX                            5374                            041511                          846 @׭��6DD1   @׭�rX��@;�Ƨ�c� ě��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�3D�Y�D��3D��3D� D�<�D���D��fD�3D�C3D��fD�ɚD��3D�<�Dڜ�D��3D�3D�6fD�D�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%�D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�Du�Dy��D��D�d)D���D���D��D�G\D��)D���D��D�M�D���D��)D���D�G\Dڧ\D���D��D�@�D�)D�m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;dA�1'A�$�A��A���A���A�I�A��A��TA���A�ƨA��jA��-A���A���A��hA�hsA�A�A���A��HA��RA�hsA�VA��A��9A�p�A�A�A�A��A�33A�ȴA�9XA�oA�  A��A��uA�-A��A��A���A�hsA�JA��A�A���A�(�A���A�5?A�ĜA�bNA���A�ĜA���A�G�A���A�{A��A�ȴA��A�G�A�(�A���A�Q�A�z�A���A���A��^A�A�A��A�z�A�
=A�M�A�5?A�oA�x�A���A���A��+A�bNA���A�\)A� �A�"�A�;dA�A�A���A���A�1'A�A��A���A��A�E�A��RA�`BA��A�v�A��-A�\)A��!A��A��A�(�A��A�;dA�O�A�Q�A�^5A�S�A�1A�&�A~�A{`BAxr�Au\)As�-Ao�^Am
=Ak�TAkC�Aj��Ai��AhVAfQ�AeAdv�Ab�A`�A_|�A]AZVAX��AXv�AW��AW"�AVA�ATĜAS�ARn�ARE�AQ�AP��AOVAM�7AL�yAK�AJ  AJ{AI�
AH �AF��AF��AF~�AFADn�AC��AB��AA/A@��A@5?A?��A>�A=
=A<��A<�A<VA;��A;dZA:�A;%A:��A9S�A7��A6��A6I�A5�#A5?}A4ȴA3XA1x�A/�mA/K�A.�!A-�-A,ȴA+�#A+C�A*��A*�RA*ffA(��A&�yA%�TA%O�A$ĜA#�^A!+A jAS�A�A�A�A�HA��A�A^5AƨA|�A�9A��A�jA�A��A�wA�A �Ap�A%A�HA��AI�A�FAVAr�A��A"�A�9AE�A�AAhsA
��A	�TA�jA��A�Al�A�yA�7A~�A1'A��A�PA�A�AVA �+A  �@���@��@���@���@�C�@�X@��9@�|�@�v�@�V@���@�5?@���@�M�@���@���@�K�@�X@���@���@�j@䛦@���@�{@��D@�K�@ݑh@�t�@ٺ^@�bN@և+@�O�@�V@���@Լj@��m@�K�@�o@ҧ�@�=q@�hs@�(�@�+@Ώ\@�E�@�`B@�%@���@�b@��@��@��#@�V@�j@�\)@���@�%@��m@��h@��@�C�@�M�@�{@��@��@���@�~�@�@�G�@��@���@��m@�33@���@��T@�&�@�Q�@���@���@��R@�$�@�V@���@���@�I�@��
@�t�@��@���@��\@�5?@���@���@�Z@�  @���@�@�O�@�O�@�7L@�V@�Q�@�  @���@�K�@��@��+@��-@��j@�b@���@�
=@�{@�G�@��@�I�@� �@���@���@���@�33@��\@�n�@�M�@�{@�`B@�(�@�C�@�"�@��@���@�^5@��@��^@�7L@���@�z�@�(�@��w@�l�@�C�@�o@�v�@�@��7@�O�@�V@���@��D@�A�@� �@�  @��;@�ƨ@���@�l�@�C�@��y@�V@�E�@�-@�$�@�{@��#@�hs@�O�@��@��D@�I�@��@��w@���@�|�@�
=@��@���@���@�{@���@��h@�%@���@�j@�Z@�A�@�b@���@��P@�\)@��@���@�n�@�M�@�5?@�-@�@��h@�/@��j@���@��D@�I�@�1'@�1'@�1'@�(�@�1@��
@��F@�t�@�33@�@��+@���@��@��T@��@�p�@�X@�%@���@��9@���@���@�Z@�1'@� �@�b@�  @��@~�y@~ff@~{@~@}�@}@}p�@}�@|�@|�/@{o@t��@n{@g��@`�@S��@L�@Fȴ@A7L@9&�@2M�@-V@'K�@"��@��@�@+@�\@�R@
�\@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�;dA�1'A�$�A��A���A���A�I�A��A��TA���A�ƨA��jA��-A���A���A��hA�hsA�A�A���A��HA��RA�hsA�VA��A��9A�p�A�A�A�A��A�33A�ȴA�9XA�oA�  A��A��uA�-A��A��A���A�hsA�JA��A�A���A�(�A���A�5?A�ĜA�bNA���A�ĜA���A�G�A���A�{A��A�ȴA��A�G�A�(�A���A�Q�A�z�A���A���A��^A�A�A��A�z�A�
=A�M�A�5?A�oA�x�A���A���A��+A�bNA���A�\)A� �A�"�A�;dA�A�A���A���A�1'A�A��A���A��A�E�A��RA�`BA��A�v�A��-A�\)A��!A��A��A�(�A��A�;dA�O�A�Q�A�^5A�S�A�1A�&�A~�A{`BAxr�Au\)As�-Ao�^Am
=Ak�TAkC�Aj��Ai��AhVAfQ�AeAdv�Ab�A`�A_|�A]AZVAX��AXv�AW��AW"�AVA�ATĜAS�ARn�ARE�AQ�AP��AOVAM�7AL�yAK�AJ  AJ{AI�
AH �AF��AF��AF~�AFADn�AC��AB��AA/A@��A@5?A?��A>�A=
=A<��A<�A<VA;��A;dZA:�A;%A:��A9S�A7��A6��A6I�A5�#A5?}A4ȴA3XA1x�A/�mA/K�A.�!A-�-A,ȴA+�#A+C�A*��A*�RA*ffA(��A&�yA%�TA%O�A$ĜA#�^A!+A jAS�A�A�A�A�HA��A�A^5AƨA|�A�9A��A�jA�A��A�wA�A �Ap�A%A�HA��AI�A�FAVAr�A��A"�A�9AE�A�AAhsA
��A	�TA�jA��A�Al�A�yA�7A~�A1'A��A�PA�A�AVA �+A  �@���@��@���@���@�C�@�X@��9@�|�@�v�@�V@���@�5?@���@�M�@���@���@�K�@�X@���@���@�j@䛦@���@�{@��D@�K�@ݑh@�t�@ٺ^@�bN@և+@�O�@�V@���@Լj@��m@�K�@�o@ҧ�@�=q@�hs@�(�@�+@Ώ\@�E�@�`B@�%@���@�b@��@��@��#@�V@�j@�\)@���@�%@��m@��h@��@�C�@�M�@�{@��@��@���@�~�@�@�G�@��@���@��m@�33@���@��T@�&�@�Q�@���@���@��R@�$�@�V@���@���@�I�@��
@�t�@��@���@��\@�5?@���@���@�Z@�  @���@�@�O�@�O�@�7L@�V@�Q�@�  @���@�K�@��@��+@��-@��j@�b@���@�
=@�{@�G�@��@�I�@� �@���@���@���@�33@��\@�n�@�M�@�{@�`B@�(�@�C�@�"�@��@���@�^5@��@��^@�7L@���@�z�@�(�@��w@�l�@�C�@�o@�v�@�@��7@�O�@�V@���@��D@�A�@� �@�  @��;@�ƨ@���@�l�@�C�@��y@�V@�E�@�-@�$�@�{@��#@�hs@�O�@��@��D@�I�@��@��w@���@�|�@�
=@��@���@���@�{@���@��h@�%@���@�j@�Z@�A�@�b@���@��P@�\)@��@���@�n�@�M�@�5?@�-@�@��h@�/@��j@���@��D@�I�@�1'@�1'@�1'@�(�@�1@��
@��F@�t�@�33@�@��+@���@��@��T@��@�p�@�X@�%@���@��9@���@���@�Z@�1'@� �@�b@�  @��@~�y@~ff@~{@~@}�@}@}p�@}�@|�@|�/@{o@t��@n{@g��@`�@S��@L�@Fȴ@A7L@9&�@2M�@-V@'K�@"��@��@�@+@�\@�R@
�\@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBJBJB	7B	7B%B  B��B��B��B��B��B��B��B�B�B��B��BBBB%BPB�B�B�B#�B&�B,B7LB<jB@�BE�BF�BE�BE�BD�BD�BC�BE�BC�B@�B=qB:^B7LB5?B1'B&�B"�B�B�B�B�BuB\B
=BB��B��B�B�B�HB�}B��B�uB�bB�7B|�Br�BcTBC�BD�BD�B<jB33B:^B\B�B&�B(�B+B!�B�B�B�B#�B+B&�B)�B#�B#�B$�B�BVB��B�TB�jB��B��B�VB�%Bk�B%�B
ƨB
�{B
{�B
u�B
��B
��B
�B
��B
�B
I�B
2-B
�B
B	�B	�TB	ɺB	�FB	�-B	�'B	�B	��B	��B	��B	�uB	�DB	~�B	v�B	l�B	]/B	N�B	F�B	A�B	<jB	7LB	0!B	'�B	�B	�B	�B	hB	DB	B��B��B��B��B	B	%B��B��B��B�B�B�TB�5B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��BȴB��B�wB�jB�^B�RB�FB�-B�B��B��B��B��B��B��B��B��B�{B�hB�JB�+B�B�B}�By�Bt�Br�Bp�Bo�Bm�Bk�BiyBgmBdZBbNB`BB^5B\)BVBO�BJ�BG�BE�BC�BB�BA�BA�B@�B@�B?}B=qB<jB;dB:^B9XB8RB8RB7LB5?B49B2-B33B2-B1'B1'B/B-B+B)�B)�B(�B(�B'�B$�B"�B!�B!�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBoBoBoBoBoBhBhBhBhBhB{B{B�B{B{B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B"�B"�B"�B"�B%�B%�B&�B%�B(�B+B,B-B+B,B/B/B33B2-B33B5?B49B5?B8RB>wB?}B?}BA�BC�BG�BJ�BM�BP�BP�BS�BS�BT�BXBYBYBM�B\)B\)B^5B^5B^5B_;B_;BcTBe`BffBiyBl�Bn�Bn�Bn�Bn�Bq�Br�Br�Bt�Bv�Bx�B}�B�B�%B�%B�7B�VB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�3B�9B�FB�^B�jB�}B��BĜBƨBǮBȴB��B��B��B��B�B�B�5B�BB�HB�NB�TB�`B�fB�mB�sB�B�B�B�B�B�B��B��B��B��B	B	B	+B	
=B	DB	JB	VB	\B	bB	oB	{B	�B	�B	�B	"�B	%�B	&�B	&�B	(�B	.B	/B	1'B	2-B	8RB	;dB	=qB	=qB	>wB	?}B	C�B	F�B	J�B	J�B	K�B	M�B	N�B	O�B	O�B	O�B	P�B	Q�B	S�B	W
B	YB	]/B	`BB	aHB	bNB	ffB	l�B	l�B	l�B	n�B	p�B	r�B	r�B	r�B	v�B	y�B	y�B	z�B	{�B	|�B	~�B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�=B	�{B	�B	��B	�;B	�B	��B
	7B
�B
#�B
-B
6FB
>wB
D�B
K�B
R�B
YB
]/B
cTB
hsB
k�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B.B2B	B	BB��B��B��B��B��B��B��B��B�B�B��B��B �B�B BB5BsB�B�B#�B&�B+�B75B<QB@iBE�BF�BE�BE�BD�BD�BC{BE�BC~B@iB=TB:BB7.B5&B1B&�B"�B�BwBvBbBZBCB
B�B��B��B�B�~B�/B�^B��B�VB�FB�B|�Br�Bc6BCyBD~BD}B<IB3B:@B;B�kB&�B(�B*�B!�B|BcB�B#�B*�B&�B)�B#�B#�B$�B�B3B��B�4B�KB��B��B�5B�BkfB%�B
ƉB
�]B
{�B
u�B
��B
�kB
��B
��B
��B
I�B
2B
B
 �B	�kB	�9B	ɣB	�.B	�B	�B	��B	��B	��B	�vB	�^B	�,B	~�B	v�B	luB	]B	N�B	F�B	AtB	<TB	79B	0B	'�B	�B	B	uB	TB	.B	�B��B��B��B��B	B	B��B��B��B�B�~B�DB�#B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��BȤB�zB�fB�YB�NB�AB�5B�B��B��B��B��B��B��B��B�}B�qB�jB�XB�9B�B�
B��B}�By�Bt�Br�Bp�Bo�Bm�BkvBijBg`BdJBb@B`3B^'B\BU�BO�BJ�BG�BE�BC�BB�BAzBA|B@tB@vB?oB=IB<\B;UB:QB9JB8EB8EB7?B52B4)B2 B3&B2 B1B1B/B-B*�B)�B)�B(�B(�B'�B$�B"�B!�B!�B!�B!�B�B�B�B{B�B~BxB�BpB�B\BsBkBfB_BGBCBGBbB?BBB?B?B@BmBPBrBlBQB�B�B�BtB�B�B�B�B�B�B�B �B!�B!�B"�B"�B"�B"�B%�B%�B&�B%�B(�B*�B+�B,�B*�B+�B/
B/B3#B2B3 B5,B4'B5,B8@B>fB?jB?iBAvBC�BG�BJ�BM�BP�BP�BS�BS�BT�BW�BYBYBM�B\B\B^B^"B^!B_'B_&Bc?BeMBfRBieBlwBn�Bn�Bn�Bn�Bq�Br�Br�Bt�Bv�Bx�B}�B��B�B�B�B�=B�QB�cB�nB�pB�uB�vB�wB�}B��B��B��B��B��B��B��B� B�	B�B�B�!B�,B�EB�PB�bB�pBĄBƌBǓBȚB̲B��B��B��B��B�B�B�%B�.B�2B�7B�CB�IB�PB�WB�gB��B�B�B�B�B��B��B��B��B	�B	B	B	
B	%B	,B	:B	>B	FB	QB	^B	dB	nB	�B	"�B	%�B	&�B	&�B	(�B	-�B	.�B	1	B	2B	83B	;FB	=PB	=SB	>WB	?]B	CuB	F�B	J�B	J�B	K�B	M�B	N�B	O�B	O�B	O�B	P�B	Q�B	S�B	V�B	X�B	]
B	`"B	a&B	b,B	fEB	ljB	ljB	lkB	nyB	p�B	r�B	r�B	r�B	v�B	y�B	y�B	z�B	{�B	|�B	~�B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�ZB	��B	ʞB	�B	�mB	��B
	B
`B
#�B
,�B
6 B
>PB
DvB
K�B
R�B
X�B
]B
c.B
hLB
k`B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.33 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708302016053117083020160531170830  AO  ARCAADJP                                                                    20160520191525    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160520191525  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160520191525  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170830  IP                  G�O�G�O�G�O�                
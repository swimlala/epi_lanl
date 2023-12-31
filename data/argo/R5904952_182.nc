CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:46Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005190546  20181005190546  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @����BԮ1   @���y] "@1�+I��c�bM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  BffB   B(  B0  B8  B?��BG��BP  BX  B`  Bh  Bo��Bw��B�  B�  B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C��C��C�  C�  C��3C��3C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C��3C��3C��3C��3C��3C�  C�  C��C��C�  C�  C�  C��C�  C�  C��C�  C�  C��3C�  C�  C��C�  C��3D � D  D� DfD�fDfD� D��Dy�D  D�fD  D� D��D� D  Dy�D��D	� D
  D
� D  D� D  D�fD  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D��D� D  D�fD  Dy�D  D� DfD� D  D� D  D� D  D� D  D� D fD �fD!  D!�fD"  D"y�D#  D#�fD$  D$y�D$��D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-y�D-��D.� D/  D/y�D0  D0� D1  D1� D2  D2�fD3fD3� D3��D4y�D5  D5� D6  D6y�D6��D7� D8  D8y�D8��D9� D:fD:� D;fD;� D;��D<y�D=  D=� D>fD>�fD?fD?�fD@  D@� DA  DA� DB  DB�fDC  DC� DD  DD�fDEfDE�fDF  DFy�DF��DG� DHfDH� DH��DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DSy�DS��DT� DUfDU�fDVfDV� DV��DW� DX  DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]�fD^  D^y�D^��D_� D`  D`�fDafDa�fDb  Db� DcfDc� Dd  Dd� De  Dey�Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl�fDm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dp��Dqy�Dq��Dry�Ds  Ds�fDtfDt� Du  Du� Dv  Dv� Dw  Dw� DwٚDy��D�FD��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@G�@�=q@�=qA�A%�AE�Ae�A��\A��\A��\A��\A�Aҏ\A�\A�\BG�B	G�BG�B�B!G�B)G�B1G�B9G�B@�HBH�HBQG�BYG�BaG�BiG�Bp�HBx�HB���B���B�p�B���B���B�p�B�p�B���B���B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�Cfk�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�5�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�5�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�5�C�(�C�)C�)C�)C�)C�)C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�5�C�(�C�(�C�5�C�(�C�(�C�)C�(�C�(�C�5�C�(�D D �{D{D�{D�D��D�D�{DD�D{D��D{D�{DD�{D{D�D	D	�{D
{D
�{D{D�{D{D��D{D�{D{D�{D{D�{D{D�{D{D�D{D�{D{D�{D{D�{D{D�{D{D�{DD�{D{D��D{D�D{D�{D�D�{D{D�{D{D�{D{D�{D{D�{D �D ��D!{D!��D"{D"�D#{D#��D${D$�D%D%�D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�D,{D,�{D-{D-�D.D.�{D/{D/�D0{D0�{D1{D1�{D2{D2��D3�D3�{D4D4�D5{D5�{D6{D6�D7D7�{D8{D8�D9D9�{D:�D:�{D;�D;�{D<D<�D={D=�{D>�D>��D?�D?��D@{D@�{DA{DA�{DB{DB��DC{DC�{DD{DD��DE�DE��DF{DF�DGDG�{DH�DH�{DIDI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP�DP�{DQ{DQ�{DR{DR�{DS{DS�DTDT�{DU�DU��DV�DV�{DWDW�{DX{DX�{DY{DY�{DZ{DZ�{D[D[�{D\{D\�{D]{D]��D^{D^�D_D_�{D`{D`��Da�Da��Db{Db�{Dc�Dc�{Dd{Dd�{De{De�Df{Df�{DgDg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl�Dl��Dm{Dm�Dn{Dn�{Do{Do�{Dp{Dp�{DqDq�DrDr�Ds{Ds��Dt�Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dw�Dy�D�PQD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A���A�A�A�bA�VA�bA�oA�oA�VA�{A��A�(�A�l�A�z�Aʉ7Aʕ�AʑhAʑhAʑhAʙ�A�x�A�K�A�bNAʉ7A�t�A�$�A�$�A�$�A�+A�33A�/A�+A�JA�
=A�JA�oAʏ\Aʙ�Aʣ�Aʧ�Aʟ�Aʺ^A��A�"�A�~�A˶FA��TA��`A˝�A�bNA�bA�M�A��/A��A�`BA���A��^A�x�A��/A�r�A���A�l�A�x�A��hA���A���A��#A��A���A���A��/A��TA�t�A��A���A��A��9A�VA��FA���A��
A�S�A��A�=qA��^A���A���A���A��`A�p�A�K�A�;dA�$�A���A���A�G�A���A�=qA�ZA��/A�C�A�^5A�x�A�Q�A���A�n�A�A��A~VAxn�At5?ArjAp�An�An(�Am+AkƨAi��AgO�Ae+Ac�TAb(�AZ�!AU`BAP��AM�
AKhsAJ{AI;dAG��ADr�A?�A>�+A> �A<�A9��A8A�A6�A4�jA333A2JA0�jA.�A,�uA+K�A*bA'�A&�jA$n�At�AffA33A~�A�A+A
��A	�A�mA��A�A��A�+A �A{A�Al�A��AAdZA�HA��Az�Ax�A �!A ff@���@��@�t�@��H@�n�@�&�@�\)@�/@��H@�7L@��D@��@@�@�@���@�"�@��@�^5@�5?@��@陚@��@�D@���@��@�b@�v�@��@�X@�j@���@�5?@܋D@۝�@�v�@���@�&�@���@ؼj@؃@�dZ@֟�@��@��/@�  @�~�@�G�@���@�b@�  @�ƨ@�;d@ΰ!@�n�@ͩ�@�9X@�+@��y@��@�V@ɑh@�V@��/@ȴ9@�bN@��;@Ə\@��@ř�@�X@�O�@�x�@�p�@�X@�G�@�/@�j@�(�@þw@öF@��;@�ƨ@�K�@�+@�ȴ@�-@��h@��@�A�@�(�@��@��m@���@��!@��@��@��@���@��7@�&�@�1@�"�@��+@��T@�@�{@�{@��#@��^@��@�&�@��j@��u@��@�1'@��
@�"�@��@�x�@���@��9@�Q�@�1'@��@�t�@�-@�&�@��@�I�@�1@�A�@�j@�z�@�I�@�\)@�o@�t�@�t�@�"�@�"�@��R@�^5@��@��@�G�@�%@��9@��D@�9X@���@�@�@��-@���@��7@��7@��7@��@�p�@�7L@��`@�1@��F@��@�t�@�dZ@�C�@�33@�"�@��H@�v�@�V@�=q@�5?@�$�@�J@���@���@���@��^@���@��h@�/@���@��@��`@��/@���@�j@��m@��@��R@��^@��7@��7@���@���@�`B@�(�@�K�@��y@���@�v�@�n�@�n�@�-@��^@�X@��D@��m@�ƨ@��F@�\)@��y@�v�@��@��@��^@�/@��`@��D@�Q�@�1'@���@���@��@��P@�l�@�;d@��@�@��H@���@���@��+@���@���@�^5@��#@���@��7@�X@�/@�Ĝ@���@�z�@�r�@�bN@�9X@�  @�ƨ@�t�@�"�@���@��!@���@��\@�v�@�$�@���@���@�x�@�hs@�hs@�?}@���@��j@�I�@��;@���@�\)@�;d@�+@��@���@���@���@�V@�@��#@���@��@�G�@���@��@� �@��w@�"�@���@���@�E�@�-@�J@���@��@��@�u%@im]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A�A�A�bA�VA�bA�oA�oA�VA�{A��A�(�A�l�A�z�Aʉ7Aʕ�AʑhAʑhAʑhAʙ�A�x�A�K�A�bNAʉ7A�t�A�$�A�$�A�$�A�+A�33A�/A�+A�JA�
=A�JA�oAʏ\Aʙ�Aʣ�Aʧ�Aʟ�Aʺ^A��A�"�A�~�A˶FA��TA��`A˝�A�bNA�bA�M�A��/A��A�`BA���A��^A�x�A��/A�r�A���A�l�A�x�A��hA���A���A��#A��A���A���A��/A��TA�t�A��A���A��A��9A�VA��FA���A��
A�S�A��A�=qA��^A���A���A���A��`A�p�A�K�A�;dA�$�A���A���A�G�A���A�=qA�ZA��/A�C�A�^5A�x�A�Q�A���A�n�A�A��A~VAxn�At5?ArjAp�An�An(�Am+AkƨAi��AgO�Ae+Ac�TAb(�AZ�!AU`BAP��AM�
AKhsAJ{AI;dAG��ADr�A?�A>�+A> �A<�A9��A8A�A6�A4�jA333A2JA0�jA.�A,�uA+K�A*bA'�A&�jA$n�At�AffA33A~�A�A+A
��A	�A�mA��A�A��A�+A �A{A�Al�A��AAdZA�HA��Az�Ax�A �!A ff@���@��@�t�@��H@�n�@�&�@�\)@�/@��H@�7L@��D@��@@�@�@���@�"�@��@�^5@�5?@��@陚@��@�D@���@��@�b@�v�@��@�X@�j@���@�5?@܋D@۝�@�v�@���@�&�@���@ؼj@؃@�dZ@֟�@��@��/@�  @�~�@�G�@���@�b@�  @�ƨ@�;d@ΰ!@�n�@ͩ�@�9X@�+@��y@��@�V@ɑh@�V@��/@ȴ9@�bN@��;@Ə\@��@ř�@�X@�O�@�x�@�p�@�X@�G�@�/@�j@�(�@þw@öF@��;@�ƨ@�K�@�+@�ȴ@�-@��h@��@�A�@�(�@��@��m@���@��!@��@��@��@���@��7@�&�@�1@�"�@��+@��T@�@�{@�{@��#@��^@��@�&�@��j@��u@��@�1'@��
@�"�@��@�x�@���@��9@�Q�@�1'@��@�t�@�-@�&�@��@�I�@�1@�A�@�j@�z�@�I�@�\)@�o@�t�@�t�@�"�@�"�@��R@�^5@��@��@�G�@�%@��9@��D@�9X@���@�@�@��-@���@��7@��7@��7@��@�p�@�7L@��`@�1@��F@��@�t�@�dZ@�C�@�33@�"�@��H@�v�@�V@�=q@�5?@�$�@�J@���@���@���@��^@���@��h@�/@���@��@��`@��/@���@�j@��m@��@��R@��^@��7@��7@���@���@�`B@�(�@�K�@��y@���@�v�@�n�@�n�@�-@��^@�X@��D@��m@�ƨ@��F@�\)@��y@�v�@��@��@��^@�/@��`@��D@�Q�@�1'@���@���@��@��P@�l�@�;d@��@�@��H@���@���@��+@���@���@�^5@��#@���@��7@�X@�/@�Ĝ@���@�z�@�r�@�bN@�9X@�  @�ƨ@�t�@�"�@���@��!@���@��\@�v�@�$�@���@���@�x�@�hs@�hs@�?}@���@��j@�I�@��;@���@�\)@�;d@�+@��@���@���@���@�V@�@��#@���@��@�G�@���@��@� �@��w@�"�@���@���@�E�@�-@�J@���@��@��@�u%@im]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	B	B	B	B	B	B	B	B	B	B	B	�B	0!B	YB	��B	�FB	�}B	ƨB	��B	��B	�B	�;B	�B	��B	�B	�`B	�B	�ZB	�ZB	�`B	�fB	�B	�B	��B	��B	��B	��B	��B
�B
%�B
0!B
7LB
@�B
p�B
��B
��B
��BJB�B0!B7LB<jBD�BL�BB�Bn�B�oB�-B��B�B�/B�fB�B��BB�B?}BO�BN�BT�Bl�Bp�BjB]/BT�BE�B0!B+B)�B(�B49B9XB(�B�BJBB��B�B�5BǮB�dB�3B��B�{B�BffB9XBhB1B
��B
�B
��B
�RB
��B
� B
k�B
K�B
�B	�fB	ƨB	�XB	��B	�=B	~�B	{�B	z�B	w�B	p�B	hsB	_;B	VB	K�B	=qB	/B	�B��B�B��BB�qB�XB�?B�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�9B�LB�^B�^B�qB��BB��B��BÖBƨBɺB��B��B��B��B��B��B��B��B��B�B�5B�#B�B�B�B�#B�B�B�
B��B��B��B��B��B�B�B�B�B�
B�B�#B�#B�/B�/B�/B�5B�;B�;B�NB�TB�yB�B�B�B�B�B�B�B��B��B��B	B	B	B	%B	JB	bB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	'�B	-B	0!B	6FB	7LB	:^B	=qB	?}B	@�B	A�B	C�B	G�B	F�B	E�B	E�B	G�B	I�B	L�B	N�B	O�B	P�B	Q�B	R�B	S�B	W
B	ZB	[#B	^5B	`BB	bNB	e`B	gmB	iyB	jB	k�B	k�B	m�B	r�B	u�B	t�B	v�B	{�B	~�B	� B	�B	~�B	{�B	z�B	|�B	�B	�B	�1B	�=B	�=B	�JB	�bB	�oB	�uB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�?B	�LB	�LB	�RB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�qB	�qB	�wB	�wB	�wB	�}B	�}B	�}B	��B	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�B	�#B	�#B	�B	�#B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�NB	�NB	�ZB	�ZB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
	7B
	7B

=B

�B
#�B
/O22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B	B	B	B	B	B	B	B	B	B	B	B	�B	0!B	YB	��B	�FB	�}B	ƨB	��B	��B	�B	�;B	�B	��B	�B	�`B	�B	�ZB	�ZB	�`B	�fB	�B	�B	��B	��B	��B	��B	��B
�B
%�B
0!B
7LB
@�B
p�B
��B
��B
��BJB�B0!B7LB<jBD�BL�BB�Bn�B�oB�-B��B�B�/B�fB�B��BB�B?}BO�BN�BT�Bl�Bp�BjB]/BT�BE�B0!B+B)�B(�B49B9XB(�B�BJBB��B�B�5BǮB�dB�3B��B�{B�BffB9XBhB1B
��B
�B
��B
�RB
��B
� B
k�B
K�B
�B	�fB	ƨB	�XB	��B	�=B	~�B	{�B	z�B	w�B	p�B	hsB	_;B	VB	K�B	=qB	/B	�B��B�B��BB�qB�XB�?B�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�9B�LB�^B�^B�qB��BB��B��BÖBƨBɺB��B��B��B��B��B��B��B��B��B�B�5B�#B�B�B�B�#B�B�B�
B��B��B��B��B��B�B�B�B�B�
B�B�#B�#B�/B�/B�/B�5B�;B�;B�NB�TB�yB�B�B�B�B�B�B�B��B��B��B	B	B	B	%B	JB	bB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	'�B	-B	0!B	6FB	7LB	:^B	=qB	?}B	@�B	A�B	C�B	G�B	F�B	E�B	E�B	G�B	I�B	L�B	N�B	O�B	P�B	Q�B	R�B	S�B	W
B	ZB	[#B	^5B	`BB	bNB	e`B	gmB	iyB	jB	k�B	k�B	m�B	r�B	u�B	t�B	v�B	{�B	~�B	� B	�B	~�B	{�B	z�B	|�B	�B	�B	�1B	�=B	�=B	�JB	�bB	�oB	�uB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�?B	�LB	�LB	�RB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�qB	�qB	�wB	�wB	�wB	�}B	�}B	�}B	��B	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�B	�#B	�#B	�B	�#B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�NB	�NB	�ZB	�ZB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
	7B
	7B

=B

�B
#�B
/O22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190546                              AO  ARCAADJP                                                                    20181005190546    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190546  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190546  QCF$                G�O�G�O�G�O�8000            
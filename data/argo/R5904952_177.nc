CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:45Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190545  20181005190545  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�婜Ġ1   @��'Ғ@1m�hr�!�c��/��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A���A�  B ffB  B  B  B ffB(ffB0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC7�fC:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C_�fCa�fCd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��3C�  C��C�  C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C��3C��3C��3D   D � DfD�fD  D� DfD� D��D� D  Dy�D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D�fD  D� DfD� D  Dy�D��Dy�D  D� D  D� D  D� D  D� D  D� D��D � D!fD!� D!��D"� D#  D#� D$  D$� D%  D%�fD&fD&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D,��D-y�D-��D.� D/fD/�fD0fD0�fD1fD1�fD2fD2�fD3  D3� D3��D4� D5  D5�fD6  D6y�D7  D7� D8  D8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@fD@� DA  DAy�DA��DBy�DB��DCy�DC��DD� DE  DE�fDF  DF� DG  DG� DHfDH� DIfDI�fDJ  DJ� DK  DK�fDL  DLy�DM  DM�fDN  DN� DN��DO� DPfDP� DP��DQy�DR  DR�fDSfDS� DT  DT�fDUfDU� DVfDV�fDWfDW� DW��DX� DYfDY�fDZfDZ� DZ��D[y�D\  D\�fD]  D]y�D]��D^� D_fD_� D`  D`�fDafDa� Db  Db� Dc  Dc�fDd  Dd� Dd��De� Df  Df� DgfDg�fDh  Dh� DifDi� Di��Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Dsy�Ds��Dt� DufDu�fDv  Dvy�Dv��Dw� Dw�3Dy��D�B�D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=q@�=qA�A%�AE�Af�RA��\A��\A��\A��\A\Aҏ\A�\)A�\B�B	G�BG�BG�B!�B)�B1G�B9G�BAG�BIG�BQG�BY�BaG�BiG�BqG�ByG�B���B���B���B���B�p�B���B���B���B��
B���B�p�B���B��
B���B���B���B���Bģ�Bȣ�Ḅ�BУ�B�p�Bأ�Bܣ�B�p�B��B��B��B��B���B�p�B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�Ck�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C68RC88RC:Q�C<k�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZk�C\Q�C^Q�C`8RCb8RCdQ�CfQ�ChQ�CjQ�ClQ�Cn8RCpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�)C�(�C�5�C�(�C�(�C�(�C�)C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�)C�)C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�)C�(�C�(�C�(�C�)C�(�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�)C�)C�)D {D �{D�D��D{D�{D�D�{DD�{D{D�D{D�{D{D�{D{D��D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D�D��D{D�{D{D�{D{D�{D{D��D{D�{D�D�{D{D�DD�D{D�{D{D�{D{D�{D{D�{D{D�{D D �{D!�D!�{D"D"�{D#{D#�{D${D$�{D%{D%��D&�D&�{D'{D'�{D({D(�{D){D)�{D*D*�{D+{D+�{D,{D,�{D-D-�D.D.�{D/�D/��D0�D0��D1�D1��D2�D2��D3{D3�{D4D4�{D5{D5��D6{D6�D7{D7�{D8{D8�{D9{D9�{D:D:�{D;{D;�{D<{D<�{D={D=��D>{D>�{D?{D?�{D@�D@�{DA{DA�DBDB�DCDC�DDDD�{DE{DE��DF{DF�{DG{DG�{DH�DH�{DI�DI��DJ{DJ�{DK{DK��DL{DL�DM{DM��DN{DN�{DODO�{DP�DP�{DQDQ�DR{DR��DS�DS�{DT{DT��DU�DU�{DV�DV��DW�DW�{DXDX�{DY�DY��DZ�DZ�{D[D[�D\{D\��D]{D]�D^D^�{D_�D_�{D`{D`��Da�Da�{Db{Db�{Dc{Dc��Dd{Dd�{DeDe�{Df{Df�{Dg�Dg��Dh{Dh�{Di�Di�{DjDj�{Dk{Dk�Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{DqDq�{Dr{Dr�{Ds{Ds�DtDt�{Du�Du��Dv{Dv�DwDw�{Dw�Dy� D�MD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aʹ9Aͺ^A͸RA͸RA;wA�ĜA�A�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A��A��/A��/A��HA��`A���A�1'AΣ�A�(�A�l�A�S�A�?}A�33A�"�A�1A��Aκ^Aκ^AΓuA�|�A�dZA�O�A�=qA��A��ÁA���A̡�A�7LA�(�Aʛ�AɮAƁA�-A�A�;dA��A��A�ĜA���A�A�A��wA��hA��A���A�(�A���A�XA���A�ƨA�I�A��A�{A��7A�`BA��hA�A�M�A�/A�S�A��A��`A��PA���A��;A���A���A�9XA���A�&�A���A�ȴA��!A�{A��A�bA�/A�M�A��A�jA�A�A�A��#A~{Azr�Aw`BAtJAs
=AmG�AjȴAi�^Af�9Adn�AbbA`�/A^v�A\��A[
=AW�mAU�PARȴAP��AO
=AL�yALAKx�AJ��AH��AF-AE
=ACƨA@��A>�\A<z�A:E�A9�^A8��A7ƨA7x�A7dZA7�A5
=A2�`A1��A0��A.A�A-7LA,ffA,-A+�#A+�hA*�A)��A)�A'�A&��A%?}A"ȴA ��A�PA��A
=A�A�jA��AbNA$�A  A�A  A�9A��AdZAM�AAjA�^A�HA�hAz�A�FA
�A
 �A	l�A	VAz�A7LAjA�A�/A�-A-AG�AoA �9@�l�@��`@�  @�C�@�~�@��@��@�\)@��@��@�R@�E�@�^@��@�u@�9@�I�@�1@��@�7@�/@��
@�\)@�+@�v�@��@�j@��@�!@�%@��m@�K�@�o@⟾@�?}@�A�@߅@ݲ-@���@�  @�+@ڧ�@٩�@��@�bN@�  @ץ�@���@�5?@�O�@��m@��y@҇+@�@Ѳ-@ёh@с@д9@�l�@��@�~�@���@�r�@�ƨ@���@���@���@���@�ȴ@�ȴ@ʗ�@�^5@�@��T@ɑh@�?}@ț�@ǝ�@��@�J@őh@Ų-@Ų-@Ų-@�O�@���@��@��@��@�/@�&�@��`@ě�@�1'@��@�\)@��@�n�@���@�x�@��@���@���@�(�@�t�@��R@��+@�V@�V@�5?@���@���@���@�p�@��j@�(�@�t�@�@���@��R@��!@���@���@�v�@�@���@�@�G�@�%@��D@��m@��@�v�@��7@�Ĝ@�  @�C�@�o@�ȴ@��H@��y@��\@�{@�n�@���@��^@��`@�bN@�b@��
@�K�@���@�C�@�~�@���@���@�I�@� �@��@���@��@��@��@��@�|�@�K�@�@���@���@�=q@���@��7@�X@�O�@�%@��@���@��u@���@�S�@���@�$�@��#@��7@�hs@�&�@��/@�z�@�  @��;@��w@�K�@�
=@���@��!@�^5@��@���@�?}@��`@�(�@��w@���@�t�@�C�@��@�o@��@���@���@��R@�n�@�5?@�{@���@�hs@�7L@�%@���@�bN@�b@��
@��w@���@�"�@���@�n�@�-@��@���@�p�@��@��`@��`@��`@��/@���@��j@��j@���@�Q�@�1@�ƨ@�\)@�ȴ@���@�5?@��@��^@��#@���@���@��7@�`B@�&�@��/@���@���@�j@�A�@�1@��w@��@�"�@��@��H@���@�x�@�/@���@��/@�z�@�Q�@�(�@�b@�  @���@���@���@�n�@�=q@�J@��@���@�@���@�O�@���@��j@��u@�(�@��@��@��@�\)@�33@��@��}@��n@n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aʹ9Aͺ^A͸RA͸RA;wA�ĜA�A�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A��A��/A��/A��HA��`A���A�1'AΣ�A�(�A�l�A�S�A�?}A�33A�"�A�1A��Aκ^Aκ^AΓuA�|�A�dZA�O�A�=qA��A��ÁA���A̡�A�7LA�(�Aʛ�AɮAƁA�-A�A�;dA��A��A�ĜA���A�A�A��wA��hA��A���A�(�A���A�XA���A�ƨA�I�A��A�{A��7A�`BA��hA�A�M�A�/A�S�A��A��`A��PA���A��;A���A���A�9XA���A�&�A���A�ȴA��!A�{A��A�bA�/A�M�A��A�jA�A�A�A��#A~{Azr�Aw`BAtJAs
=AmG�AjȴAi�^Af�9Adn�AbbA`�/A^v�A\��A[
=AW�mAU�PARȴAP��AO
=AL�yALAKx�AJ��AH��AF-AE
=ACƨA@��A>�\A<z�A:E�A9�^A8��A7ƨA7x�A7dZA7�A5
=A2�`A1��A0��A.A�A-7LA,ffA,-A+�#A+�hA*�A)��A)�A'�A&��A%?}A"ȴA ��A�PA��A
=A�A�jA��AbNA$�A  A�A  A�9A��AdZAM�AAjA�^A�HA�hAz�A�FA
�A
 �A	l�A	VAz�A7LAjA�A�/A�-A-AG�AoA �9@�l�@��`@�  @�C�@�~�@��@��@�\)@��@��@�R@�E�@�^@��@�u@�9@�I�@�1@��@�7@�/@��
@�\)@�+@�v�@��@�j@��@�!@�%@��m@�K�@�o@⟾@�?}@�A�@߅@ݲ-@���@�  @�+@ڧ�@٩�@��@�bN@�  @ץ�@���@�5?@�O�@��m@��y@҇+@�@Ѳ-@ёh@с@д9@�l�@��@�~�@���@�r�@�ƨ@���@���@���@���@�ȴ@�ȴ@ʗ�@�^5@�@��T@ɑh@�?}@ț�@ǝ�@��@�J@őh@Ų-@Ų-@Ų-@�O�@���@��@��@��@�/@�&�@��`@ě�@�1'@��@�\)@��@�n�@���@�x�@��@���@���@�(�@�t�@��R@��+@�V@�V@�5?@���@���@���@�p�@��j@�(�@�t�@�@���@��R@��!@���@���@�v�@�@���@�@�G�@�%@��D@��m@��@�v�@��7@�Ĝ@�  @�C�@�o@�ȴ@��H@��y@��\@�{@�n�@���@��^@��`@�bN@�b@��
@�K�@���@�C�@�~�@���@���@�I�@� �@��@���@��@��@��@��@�|�@�K�@�@���@���@�=q@���@��7@�X@�O�@�%@��@���@��u@���@�S�@���@�$�@��#@��7@�hs@�&�@��/@�z�@�  @��;@��w@�K�@�
=@���@��!@�^5@��@���@�?}@��`@�(�@��w@���@�t�@�C�@��@�o@��@���@���@��R@�n�@�5?@�{@���@�hs@�7L@�%@���@�bN@�b@��
@��w@���@�"�@���@�n�@�-@��@���@�p�@��@��`@��`@��`@��/@���@��j@��j@���@�Q�@�1@�ƨ@�\)@�ȴ@���@�5?@��@��^@��#@���@���@��7@�`B@�&�@��/@���@���@�j@�A�@�1@��w@��@�"�@��@��H@���@�x�@�/@���@��/@�z�@�Q�@�(�@�b@�  @���@���@���@�n�@�=q@�J@��@���@�@���@�O�@���@��j@��u@�(�@��@��@��@�\)@�33@��@��}@��n@n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BG�BG�BG�BG�BG�BH�BH�BH�BH�BH�BH�BH�BH�BH�BH�BI�BI�BH�BI�BI�BI�BI�BJ�BK�BM�BM�BO�BR�Bk�B�
B	w�B	ɺB
B
2-B
=qB
H�B
T�B
e`B
�\B
��B
�BJB{B�B�B �B/B;dB@�BL�BN�BVBaHBffBn�B�bB��B��B��BƨBɺB��B��BĜB�dB�jBƨB�ZB��B{B&�B-B33B5?B49B1'B.B,B'�B �BbB�B�TB��B�B��B��Bz�B_;BN�B&�BB
�B
��B
�XB
��B
�B
r�B
ffB
VB
@�B
uB	��B	�B	�fB	��B	�^B	��B	�PB	x�B	m�B	T�B	I�B	D�B	49B	'�B	!�B	�B	PB	B��B�B�NB�)B�B��BɺBǮBŢBBǮBɺBŢB�wB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�3B�9B�LB�dB�jB��BBÖBÖBÖBǮB��B��B��B�
B�B�B�B�B�5B�;B�5B�/B�)B�B�B�B��B��B��B��B��B��B��B�5B�HB�HB�NB�ZB�ZB�sB�mB�mB�sB�yB�yB�B�yB�yB�B�B�B�B�B�B�B��B�B��B��B��B��B	B	1B	PB	\B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	%�B	'�B	(�B	+B	,B	,B	,B	-B	.B	/B	7LB	?}B	A�B	A�B	@�B	@�B	D�B	E�B	J�B	N�B	O�B	P�B	Q�B	S�B	T�B	XB	YB	[#B	[#B	\)B	]/B	^5B	]/B	_;B	`BB	aHB	bNB	dZB	e`B	hsB	k�B	l�B	n�B	o�B	q�B	r�B	s�B	t�B	u�B	x�B	z�B	z�B	z�B	z�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�=B	�JB	�VB	�VB	�VB	�JB	�\B	�\B	�\B	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�3B	�3B	�3B	�3B	�3B	�9B	�?B	�?B	�?B	�?B	�LB	�jB	�jB	�qB	�qB	�wB	�}B	�wB	�wB	�}B	��B	��B	��B	ŢB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�fB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
1B
�B
"�B
+Q222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BG�BG�BG�BG�BG�BH�BH�BH�BH�BH�BH�BH�BH�BH�BH�BI�BI�BH�BI�BI�BI�BI�BJ�BK�BM�BM�BO�BR�Bk�B�
B	w�B	ɺB
B
2-B
=qB
H�B
T�B
e`B
�\B
��B
�BJB{B�B�B �B/B;dB@�BL�BN�BVBaHBffBn�B�bB��B��B��BƨBɺB��B��BĜB�dB�jBƨB�ZB��B{B&�B-B33B5?B49B1'B.B,B'�B �BbB�B�TB��B�B��B��Bz�B_;BN�B&�BB
�B
��B
�XB
��B
�B
r�B
ffB
VB
@�B
uB	��B	�B	�fB	��B	�^B	��B	�PB	x�B	m�B	T�B	I�B	D�B	49B	'�B	!�B	�B	PB	B��B�B�NB�)B�B��BɺBǮBŢBBǮBɺBŢB�wB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�3B�9B�LB�dB�jB��BBÖBÖBÖBǮB��B��B��B�
B�B�B�B�B�5B�;B�5B�/B�)B�B�B�B��B��B��B��B��B��B��B�5B�HB�HB�NB�ZB�ZB�sB�mB�mB�sB�yB�yB�B�yB�yB�B�B�B�B�B�B�B��B�B��B��B��B��B	B	1B	PB	\B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	%�B	'�B	(�B	+B	,B	,B	,B	-B	.B	/B	7LB	?}B	A�B	A�B	@�B	@�B	D�B	E�B	J�B	N�B	O�B	P�B	Q�B	S�B	T�B	XB	YB	[#B	[#B	\)B	]/B	^5B	]/B	_;B	`BB	aHB	bNB	dZB	e`B	hsB	k�B	l�B	n�B	o�B	q�B	r�B	s�B	t�B	u�B	x�B	z�B	z�B	z�B	z�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�=B	�JB	�VB	�VB	�VB	�JB	�\B	�\B	�\B	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�3B	�3B	�3B	�3B	�3B	�9B	�?B	�?B	�?B	�?B	�LB	�jB	�jB	�qB	�qB	�wB	�}B	�wB	�wB	�}B	��B	��B	��B	ŢB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�fB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
1B
�B
"�B
+Q222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190545                              AO  ARCAADJP                                                                    20181005190545    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190545  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190545  QCF$                G�O�G�O�G�O�8000            
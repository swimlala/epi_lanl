CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:59Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190559  20181005190559  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @������1   @���[�P@0�S����c��;dZ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A���A���A���A���A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�33B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCG�fCI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D��D� D  D� D  D� D��D� DfD�fD	  D	� D
  D
� D  D� DfD� D  Dy�D��D� DfD� D��D� DfD� D  D� D  D� D  Dy�D��Dy�D  D� DfD� D  D� D  D� DfD� D  D� D  D� DfD� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5y�D6  D6� D7  D7� D8  D8� D9fD9� D9��D:� D;  D;�fD<fD<� D=  D=� D>  D>� D>��D?� D@  D@� DAfDA�fDB  DBy�DC  DC� DD  DDy�DE  DE� DF  DFy�DG  DG�fDHfDH�fDIfDI�fDJfDJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQfDQ� DQ��DR� DS  DS� DT  DT� DU  DU�fDV  DV� DV��DWy�DW��DX� DYfDY�fDZfDZ�fD[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� DcfDc� Dd  Dd�fDe  De� DffDf� Dg  Dg� Dh  Dh�fDifDi� Dj  Dj� Dj��Dk� Dl  Dl�fDmfDm� Dn  Dn� Do  Do� Dp  Dp�fDqfDq�fDrfDr�fDs  Dsy�Ds��Dty�Dt��Duy�Du��Dv� DwfDw� Dw� Dy�HD�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�=qA�A%�AE�Ae�A�\)A�\)A�\)A�\)A�\)Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B��
B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�p�B���Bģ�B��
B��
B��
Bԣ�Bأ�Bܣ�B��B�p�B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C8RC Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CF8RCH8RCJ8RCLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�)C�)C�)C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�DD�{D{D�{D{D�{DD�{D�D��D	{D	�{D
{D
�{D{D�{D�D�{D{D�DD�{D�D�{DD�{D�D�{D{D�{D{D�{D{D�DD�D{D�{D�D�{D{D�{D{D�{D�D�{D{D�{D{D�{D�D�{DD�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&D&�{D'{D'�{D({D(�{D)�D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5D5�D6{D6�{D7{D7�{D8{D8�{D9�D9�{D:D:�{D;{D;��D<�D<�{D={D=�{D>{D>�{D?D?�{D@{D@�{DA�DA��DB{DB�DC{DC�{DD{DD�DE{DE�{DF{DF�DG{DG��DH�DH��DI�DI��DJ�DJ��DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP��DQ�DQ�{DRDR�{DS{DS�{DT{DT�{DU{DU��DV{DV�{DWDW�DXDX�{DY�DY��DZ�DZ��D[�D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{DaDa�{Db{Db�{Dc�Dc�{Dd{Dd��De{De�{Df�Df�{Dg{Dg�{Dh{Dh��Di�Di�{Dj{Dj�{DkDk�{Dl{Dl��Dm�Dm�{Dn{Dn�{Do{Do�{Dp{Dp��Dq�Dq��Dr�Dr��Ds{Ds�DtDt�DuDu�DvDv�{Dw�Dw�{Dw�{Dy��D�J=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��
A��/A��/A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A���A���A���A�1A�Aȗ�A�AǴ9Aǝ�AǑhAǃA�I�A���A��mA��HA���AƍPA�JAŁA�1A�XA��yAç�Aã�A�A��A�1A� �A�t�A���A���A�/A���A��A�^5A�K�A�ZA���A��A��HA�jA�7LA��HA���A�;dA��TA�=qA�ƨA��jA��+A�G�A�9XA��hA��A�A��A�"�A��/A�r�A��HA��A�A�A�K�A��7A�t�A��A��/A���A��RA�  A�~�A�9XA��FA�ƨA�oA��A�"�A���A�jA���A��-A��+A�dZA�E�A���A���A��A��A%Aw��Ar�\Am�Ai�-Ac�A_�A^5?A\VAW�AQ�
AN�9AM�
AL�RAJ��AHffADȴAB-AA|�A@�A@n�A@JA=�^A:�jA8��A6��A4�A3�A2�yA2jA1�hA/�^A-�hA+S�A)XA(bNA'`BA%
=A#�hA!�A!&�A =qA��A%AJA��Al�A{A�#A|�AVA�uA�A?}AVA��A��A��A��AXAr�A�A
=A	ƨA�+AbA�^At�A�\Ap�A��AM�A��A�A b@�ƨ@���@�K�@�
=@�p�@���@��`@���@�7L@���@��F@�ff@�1'@��F@�C�@��y@��T@���@�-@�?}@�"�@�j@�@�^5@�^@���@�ff@�X@�@�bN@�A�@��@��@�&�@��@߶F@��`@�
=@ڸR@�$�@�p�@�9X@�dZ@�S�@�\)@���@��`@ӝ�@�"�@��y@���@�v�@�=q@�E�@�{@ёh@���@ЋD@�+@��T@�x�@̓u@�;d@�@�hs@ɉ7@�`B@ǶF@��@�K�@���@Ų-@�/@ċD@�9X@î@�@�@�x�@��/@�9X@��@�K�@���@�=q@�=q@�5?@���@�hs@��`@��D@�(�@�  @��@��@��!@�J@���@���@��!@��+@��@�x�@�G�@��@���@�bN@�1'@��@�@�@�ȴ@�M�@�V@��\@��\@�E�@��@���@���@��@���@�V@�E�@�J@�p�@��@��@�I�@���@�;d@�33@�33@�o@���@�E�@�J@��-@��7@�x�@�/@� �@�|�@�;d@�+@��H@��\@�n�@�V@�-@��@��@��@�/@��j@���@�z�@���@���@��R@�V@�-@���@���@��h@��@��@��j@��j@���@��@�hs@�G�@��@��@��@�b@��;@�t�@��R@�V@�5?@�J@��@�x�@��/@��`@��j@��@���@���@�S�@���@��\@�J@���@�`B@�G�@�G�@�7L@�%@���@�Q�@�(�@��F@��@��@��H@���@�5?@���@�x�@�`B@�O�@�O�@�/@�r�@�Q�@�I�@�A�@�(�@�b@��
@�|�@�C�@��@���@�V@��@��^@�p�@�X@�%@���@�r�@���@��;@�ƨ@��@�t�@�\)@�S�@�33@�"�@�ȴ@���@��\@�n�@�5?@��@��@���@���@�Ĝ@��u@�bN@�I�@�A�@���@���@�\)@��@��@��@�@��H@���@��H@��\@�M�@��#@�`B@�/@���@�bN@�A�@�A�@�j@��
@���@��;@��@�A�@�A�@��@�|�@�33@��@���@���@�M�@��#@�hs@�G�@��@���@��@���@��u@��@�bN@�9X@��@�b@��;@���@�t�@�S�@��@��y@�~�@�J@���@�`B@�!-@y�>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A��
A��/A��/A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A���A���A���A�1A�Aȗ�A�AǴ9Aǝ�AǑhAǃA�I�A���A��mA��HA���AƍPA�JAŁA�1A�XA��yAç�Aã�A�A��A�1A� �A�t�A���A���A�/A���A��A�^5A�K�A�ZA���A��A��HA�jA�7LA��HA���A�;dA��TA�=qA�ƨA��jA��+A�G�A�9XA��hA��A�A��A�"�A��/A�r�A��HA��A�A�A�K�A��7A�t�A��A��/A���A��RA�  A�~�A�9XA��FA�ƨA�oA��A�"�A���A�jA���A��-A��+A�dZA�E�A���A���A��A��A%Aw��Ar�\Am�Ai�-Ac�A_�A^5?A\VAW�AQ�
AN�9AM�
AL�RAJ��AHffADȴAB-AA|�A@�A@n�A@JA=�^A:�jA8��A6��A4�A3�A2�yA2jA1�hA/�^A-�hA+S�A)XA(bNA'`BA%
=A#�hA!�A!&�A =qA��A%AJA��Al�A{A�#A|�AVA�uA�A?}AVA��A��A��A��AXAr�A�A
=A	ƨA�+AbA�^At�A�\Ap�A��AM�A��A�A b@�ƨ@���@�K�@�
=@�p�@���@��`@���@�7L@���@��F@�ff@�1'@��F@�C�@��y@��T@���@�-@�?}@�"�@�j@�@�^5@�^@���@�ff@�X@�@�bN@�A�@��@��@�&�@��@߶F@��`@�
=@ڸR@�$�@�p�@�9X@�dZ@�S�@�\)@���@��`@ӝ�@�"�@��y@���@�v�@�=q@�E�@�{@ёh@���@ЋD@�+@��T@�x�@̓u@�;d@�@�hs@ɉ7@�`B@ǶF@��@�K�@���@Ų-@�/@ċD@�9X@î@�@�@�x�@��/@�9X@��@�K�@���@�=q@�=q@�5?@���@�hs@��`@��D@�(�@�  @��@��@��!@�J@���@���@��!@��+@��@�x�@�G�@��@���@�bN@�1'@��@�@�@�ȴ@�M�@�V@��\@��\@�E�@��@���@���@��@���@�V@�E�@�J@�p�@��@��@�I�@���@�;d@�33@�33@�o@���@�E�@�J@��-@��7@�x�@�/@� �@�|�@�;d@�+@��H@��\@�n�@�V@�-@��@��@��@�/@��j@���@�z�@���@���@��R@�V@�-@���@���@��h@��@��@��j@��j@���@��@�hs@�G�@��@��@��@�b@��;@�t�@��R@�V@�5?@�J@��@�x�@��/@��`@��j@��@���@���@�S�@���@��\@�J@���@�`B@�G�@�G�@�7L@�%@���@�Q�@�(�@��F@��@��@��H@���@�5?@���@�x�@�`B@�O�@�O�@�/@�r�@�Q�@�I�@�A�@�(�@�b@��
@�|�@�C�@��@���@�V@��@��^@�p�@�X@�%@���@�r�@���@��;@�ƨ@��@�t�@�\)@�S�@�33@�"�@�ȴ@���@��\@�n�@�5?@��@��@���@���@�Ĝ@��u@�bN@�I�@�A�@���@���@�\)@��@��@��@�@��H@���@��H@��\@�M�@��#@�`B@�/@���@�bN@�A�@�A�@�j@��
@���@��;@��@�A�@�A�@��@�|�@�33@��@���@���@�M�@��#@�hs@�G�@��@���@��@���@��u@��@�bN@�9X@��@�b@��;@���@�t�@�S�@��@��y@�~�@�J@���@�`B@�!-@y�>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B+B+B)�B+B)�B)�B)�B)�B)�B)�B)�B)�B)�B+B+B+B+B+B+B+B-B<jBL�B��B	!�B	A�B	I�B	P�B	XB	w�B	��B	��B	��B	�B	ŢB	��B
�B
jB
�7B
��B
�dB
ÖB
�B:^BA�BJ�B_;BcTBcTBs�B�B�DB�\B�\B��B�-B��B�ZB+B�B:^BI�B9XB0!B/B-B.B7LB0!B$�B�B�B\B	7B%BB��B��B�B�`B�BB�BȴB�jB�-B�B��Bz�Be`BN�B!�BB
��B
�B
��B
�-B
��B
�B
q�B
[#B
H�B
9XB
1'B
"�B
�B
+B	�`B	�FB	�oB	n�B	O�B	%�B	\B	B��B�HBƨB�dB�FB�'B��B��B��B�{B�uB�oB�oB�bB�hB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LB�^B�LB�?B�LB�qBB��B�^B�^B�dB�jB�}BBBB��BB��B��B�}B�}B��B��B��B��BƨBɺB��B�RB�9B�-B�-B�'B�!B�B�?B�qBÖBǮB��B��B��BȴBȴBǮBƨBÖB��B��BƨB��B��B��B��B��B��B��B��B�
B�
B�B�B�;B�HB�HB�NB�mB�yB�B�B�B�B�B�B�B��B��B��B��B	  B	B	B		7B	uB	�B	�B	�B	"�B	'�B	'�B	'�B	%�B	#�B	(�B	+B	/B	0!B	33B	<jB	?}B	C�B	J�B	K�B	N�B	P�B	Q�B	Q�B	VB	W
B	YB	ZB	ZB	ZB	[#B	\)B	\)B	[#B	\)B	]/B	_;B	aHB	cTB	cTB	e`B	l�B	n�B	n�B	t�B	v�B	y�B	{�B	� B	�B	�B	�+B	�DB	�=B	�7B	�1B	�+B	�7B	�7B	�PB	�\B	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�FB	�XB	�jB	�wB	�wB	�}B	��B	�}B	��B	��B	B	B	��B	B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�HB	�`B	�ZB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
PB
JB
JB
JB
PB
PB
VB
\B
VB
VB
\B
bB
hB
oB
oB
oB
{B
{B
uB
oB
oB
oB
oB
oB
oB
uB
�B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
#�B
5%22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B+B+B)�B+B)�B)�B)�B)�B)�B)�B)�B)�B)�B+B+B+B+B+B+B+B-B<jBL�B��B	!�B	A�B	I�B	P�B	XB	w�B	��B	��B	��B	�B	ŢB	��B
�B
jB
�7B
��B
�dB
ÖB
�B:^BA�BJ�B_;BcTBcTBs�B�B�DB�\B�\B��B�-B��B�ZB+B�B:^BI�B9XB0!B/B-B.B7LB0!B$�B�B�B\B	7B%BB��B��B�B�`B�BB�BȴB�jB�-B�B��Bz�Be`BN�B!�BB
��B
�B
��B
�-B
��B
�B
q�B
[#B
H�B
9XB
1'B
"�B
�B
+B	�`B	�FB	�oB	n�B	O�B	%�B	\B	B��B�HBƨB�dB�FB�'B��B��B��B�{B�uB�oB�oB�bB�hB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LB�^B�LB�?B�LB�qBB��B�^B�^B�dB�jB�}BBBB��BB��B��B�}B�}B��B��B��B��BƨBɺB��B�RB�9B�-B�-B�'B�!B�B�?B�qBÖBǮB��B��B��BȴBȴBǮBƨBÖB��B��BƨB��B��B��B��B��B��B��B��B�
B�
B�B�B�;B�HB�HB�NB�mB�yB�B�B�B�B�B�B�B��B��B��B��B	  B	B	B		7B	uB	�B	�B	�B	"�B	'�B	'�B	'�B	%�B	#�B	(�B	+B	/B	0!B	33B	<jB	?}B	C�B	J�B	K�B	N�B	P�B	Q�B	Q�B	VB	W
B	YB	ZB	ZB	ZB	[#B	\)B	\)B	[#B	\)B	]/B	_;B	aHB	cTB	cTB	e`B	l�B	n�B	n�B	t�B	v�B	y�B	{�B	� B	�B	�B	�+B	�DB	�=B	�7B	�1B	�+B	�7B	�7B	�PB	�\B	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�FB	�XB	�jB	�wB	�wB	�}B	��B	�}B	��B	��B	B	B	��B	B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�HB	�`B	�ZB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
PB
JB
JB
JB
PB
PB
VB
\B
VB
VB
\B
bB
hB
oB
oB
oB
{B
{B
uB
oB
oB
oB
oB
oB
oB
uB
�B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
#�B
5%22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190559                              AO  ARCAADJP                                                                    20181005190559    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190559  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190559  QCF$                G�O�G�O�G�O�8000            
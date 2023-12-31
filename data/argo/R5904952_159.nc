CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:41Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190541  20181005190541  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)����1   @��*W:��@0������c��l�C�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   AA��A`  A�  A�  A�  A�  A���A�  A�  A���B   B  BffBffB   B(ffB0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�33C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0�C2  C4  C6  C8  C:  C<  C=�fC@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C��3C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D fD � D ��D� DfD� D��D� D  D� D  D� D  D� D��Dy�D  D� D	  D	� D
fD
� DfD�fDfD� D��D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  Dy�D  D� D  D� D  D� DfD� D  D� D��D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&fD&�fD'  D'� D(  D(y�D)  D)� D*  D*�fD+  D+� D,  D,y�D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3fD3� D4  D4� D5fD5�fD6  D6y�D7  D7�fD8  D8y�D9fD9� D:  D:� D;  D;� D;��D<� D=  D=� D>  D>y�D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DG� DH  DH� DI  DI� DJ  DJy�DK  DKy�DL  DL� DMfDM� DN  DN� DN��DO� DP  DPy�DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[�fD\  D\y�D]  D]�fD^fD^�fD_fD_�fD`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dg��Dhy�Di  Di� Dj  Dj� Dk  Dk�fDlfDl�fDm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dx  Dxs3D�K�D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@�=qA�A%�AF�RAe�A��\A��\A��\A��\A�\)Aҏ\A�\A�\)BG�B	G�B�B�B!G�B)�B1�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B���B���B�p�B���B���B�p�B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bݣ�Bߣ�B��B��B�p�B�p�B���B���B��
C k�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.k�C0k�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>8RC@Q�CBk�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�5�C�(�C�)C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�)C�)C�)C�(�C�)C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�)C�)C�(�C�5�C�(�C�)C�(�C�(�C�)C�(�C�(�C�(�C�5�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�)C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�D �D �{DD�{D�D�{DD�{D{D�{D{D�{D{D�{DD�D{D�{D	{D	�{D
�D
�{D�D��D�D�{DD�{D{D�{D{D�{D{D�{D{D��D{D�{D{D�{D{D�{D{D�{D{D�{D{D�D{D�{D{D�{D{D�D{D�{D{D�{D{D�{D�D�{D{D�{D D �{D!{D!�{D"{D"��D#{D#�{D${D$�{D%{D%�{D&�D&��D'{D'�{D({D(�D){D)�{D*{D*��D+{D+�{D,{D,�D-{D-�{D.D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2��D3�D3�{D4{D4�{D5�D5��D6{D6�D7{D7��D8{D8�D9�D9�{D:{D:�{D;{D;�{D<D<�{D={D=�{D>{D>�D?{D?�{D@{D@��DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DGDG�{DH{DH�{DI{DI�{DJ{DJ�DK{DK�DL{DL�{DM�DM�{DN{DN�{DODO�{DP{DP�DQ{DQ��DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[�D[��D\{D\�D]{D]��D^�D^��D_�D_��D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd�Dd�{De{De�{Df{Df�{Dg{Dg�{DhDh�Di{Di�{Dj{Dj�{Dk{Dk��Dl�Dl��Dm{Dm�{Dn{Dn�{Do{Do�Dp{Dp�{Dq{Dq�Dr{Dr�{Ds{Ds�{Dt{Dt�DuDu�DvDv�DwDw�Dx{Dx��D�U�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S�A�S�A�VA�VA�VA�VA�Q�A�O�A�VA�VA�K�A�G�A�?}A�A�A�?}A�=qA�;dA�"�A��A�bA�VA�
=A���A��`A��
AѮAџ�AёhAэPAэPAуA�`BA��HA���A���Aд9AЮA�oAϋDA�t�A�VA��AμjA�ffA�A��`A���A�%A�oA��A��A��A�
=A�/A�1A�9XAhA�jA��DA��A�l�A��A��A�hsA�JA�G�A���A��A�S�A�%A��PA���A��7A��
A�/A�A�l�A�  A�$�A��jA�A�|�A�x�A���A�&�A���A��9A��A��HA���A�33A�7LA�t�A��jA���A��7A�bNA��
A�5?A��A�XA��yAC�A}�;A{�Ax�yAuO�Ar�Aq�wAp �Am��AlAj�\AhA�Af��Adz�Aa�A]�FA[��A[7LAYS�AU��ASC�AQ��AN�RAK��AIAE`BAB�+A@z�A>��A=l�A=33A<=qA;&�A:�A8�!A6�yA5?}A3ƨA2�A1�A0A�A.�DA, �A*�/A)�A)��A)+A(5?A'p�A'�A%��A$�A$  A"�/A!�-A ��A�-A��A1A�AffA��AJA��A�uA�A��A�!A��A
=A��A�uA��A�RA�+A�A�A��A�PA1A
VA	hsAz�A��A��A�A
=AjAn�A�uA��AQ�A\)AVA�#AO�A �@�n�A b@���@��`@��@�/@�b@�K�@�=q@���@� �@�t�@�@��@�@�1@@�&�@�dZ@���@�v�@�$�@�X@�j@�@�hs@�j@�C�@�V@��@��@�
=@�-@�hs@��@؋D@��@ץ�@�K�@�`B@��/@�S�@�ȴ@�@��`@�j@�bN@�1@ϥ�@�+@��H@�ȴ@Ώ\@�@Ͳ-@���@�  @���@�5?@��T@�p�@�%@�G�@��@�K�@�"�@�
=@��@��y@�^5@�@š�@�&�@ēu@�  @�ƨ@Å@�\)@�33@���@�ff@��@��h@�X@���@���@�z�@��
@�+@��R@���@���@���@�~�@��^@���@�A�@�S�@���@�~�@�$�@��T@�hs@�Ĝ@���@�C�@�v�@��#@���@��-@���@��h@��@���@�A�@���@�
=@��@���@�M�@���@��@��@�\)@�+@��H@�~�@�E�@���@�x�@��/@�z�@�Q�@�Q�@�j@�1'@��m@�dZ@�33@�-@���@��h@�/@���@��u@�(�@�1@��m@���@���@�"�@��@���@�n�@�V@��@�`B@�V@�V@���@���@��`@���@��D@�bN@�I�@��;@���@��@���@�V@�-@�@��^@�hs@���@��`@�Ĝ@�bN@��
@�33@�"�@�"�@���@���@�@��H@��y@��H@���@��!@��!@�M�@���@���@�X@�G�@���@�Q�@�1@���@���@�l�@�
=@�@��y@���@��-@�x�@���@��j@��D@� �@��F@�C�@�@���@��\@�J@���@�p�@�G�@�%@���@�(�@�b@��@��w@��P@�t�@�C�@�
=@��H@���@�^5@�J@���@���@��7@�G�@���@��@�r�@� �@���@�ƨ@��@�t�@�C�@�ȴ@�E�@��h@�/@�%@��/@��9@�Z@��
@��@�S�@���@��!@���@�n�@�@���@��h@�x�@�G�@��@�V@�%@��`@��j@��@�Z@�  @�1@�1@���@�C�@�+@�ȴ@�n�@�=q@��@�J@���@��T@���@���@���@���@���@�r�@�r�@��D@s�@c�F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�S�A�S�A�VA�VA�VA�VA�Q�A�O�A�VA�VA�K�A�G�A�?}A�A�A�?}A�=qA�;dA�"�A��A�bA�VA�
=A���A��`A��
AѮAџ�AёhAэPAэPAуA�`BA��HA���A���Aд9AЮA�oAϋDA�t�A�VA��AμjA�ffA�A��`A���A�%A�oA��A��A��A�
=A�/A�1A�9XAhA�jA��DA��A�l�A��A��A�hsA�JA�G�A���A��A�S�A�%A��PA���A��7A��
A�/A�A�l�A�  A�$�A��jA�A�|�A�x�A���A�&�A���A��9A��A��HA���A�33A�7LA�t�A��jA���A��7A�bNA��
A�5?A��A�XA��yAC�A}�;A{�Ax�yAuO�Ar�Aq�wAp �Am��AlAj�\AhA�Af��Adz�Aa�A]�FA[��A[7LAYS�AU��ASC�AQ��AN�RAK��AIAE`BAB�+A@z�A>��A=l�A=33A<=qA;&�A:�A8�!A6�yA5?}A3ƨA2�A1�A0A�A.�DA, �A*�/A)�A)��A)+A(5?A'p�A'�A%��A$�A$  A"�/A!�-A ��A�-A��A1A�AffA��AJA��A�uA�A��A�!A��A
=A��A�uA��A�RA�+A�A�A��A�PA1A
VA	hsAz�A��A��A�A
=AjAn�A�uA��AQ�A\)AVA�#AO�A �@�n�A b@���@��`@��@�/@�b@�K�@�=q@���@� �@�t�@�@��@�@�1@@�&�@�dZ@���@�v�@�$�@�X@�j@�@�hs@�j@�C�@�V@��@��@�
=@�-@�hs@��@؋D@��@ץ�@�K�@�`B@��/@�S�@�ȴ@�@��`@�j@�bN@�1@ϥ�@�+@��H@�ȴ@Ώ\@�@Ͳ-@���@�  @���@�5?@��T@�p�@�%@�G�@��@�K�@�"�@�
=@��@��y@�^5@�@š�@�&�@ēu@�  @�ƨ@Å@�\)@�33@���@�ff@��@��h@�X@���@���@�z�@��
@�+@��R@���@���@���@�~�@��^@���@�A�@�S�@���@�~�@�$�@��T@�hs@�Ĝ@���@�C�@�v�@��#@���@��-@���@��h@��@���@�A�@���@�
=@��@���@�M�@���@��@��@�\)@�+@��H@�~�@�E�@���@�x�@��/@�z�@�Q�@�Q�@�j@�1'@��m@�dZ@�33@�-@���@��h@�/@���@��u@�(�@�1@��m@���@���@�"�@��@���@�n�@�V@��@�`B@�V@�V@���@���@��`@���@��D@�bN@�I�@��;@���@��@���@�V@�-@�@��^@�hs@���@��`@�Ĝ@�bN@��
@�33@�"�@�"�@���@���@�@��H@��y@��H@���@��!@��!@�M�@���@���@�X@�G�@���@�Q�@�1@���@���@�l�@�
=@�@��y@���@��-@�x�@���@��j@��D@� �@��F@�C�@�@���@��\@�J@���@�p�@�G�@�%@���@�(�@�b@��@��w@��P@�t�@�C�@�
=@��H@���@�^5@�J@���@���@��7@�G�@���@��@�r�@� �@���@�ƨ@��@�t�@�C�@�ȴ@�E�@��h@�/@�%@��/@��9@�Z@��
@��@�S�@���@��!@���@�n�@�@���@��h@�x�@�G�@��@�V@�%@��`@��j@��@�Z@�  @�1@�1@���@�C�@�+@�ȴ@�n�@�=q@��@�J@���@��T@���@���@���@���@���@�r�@�r�@��D@s�@c�F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB7LB7LB:^B?}BA�BI�BK�BL�BL�BK�BJ�BL�B]/B`BBbNBbNBe`Bn�Bp�Bm�Bl�Bq�Bp�Bo�Bo�By�B�oB��B��B��B�B�B�B�!BB�B��BB�BN�BZBdZBe`BffBffBhsBl�Bk�BjBl�Bt�Bz�B�B�B~�Bm�B]/BW
BM�B=qB7LB33B"�B��B�#B�B~�Be`BR�B>wB�BbBB
�fB
B
�VB
ffB
W
B
K�B
5?B
�B	�B	��B	��B	�B	��B	��B	�oB	�PB	�+B	{�B	r�B	k�B	_;B	VB	L�B	=qB	/B	%�B	�B	uB	B�B�TB��BÖB��B�XB�B��B��B��B�B�B�B��B��B�-B�'B�3B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B�B��B��B��B��B��B��B��B�B�B�LB�dB�qB��BĜBÖBŢBŢBŢBÖB��B�B�
B�/B�BB�HB�NB�`B�`B�yB�yB�yB�B�B�B�yB�B�B�B�B�yB�sB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B	B	%B	PB	\B	oB	uB	�B	�B	�B	�B	�B	"�B	%�B	'�B	'�B	+B	/B	1'B	33B	5?B	8RB	9XB	8RB	6FB	8RB	=qB	=qB	<jB	;dB	;dB	<jB	A�B	E�B	G�B	J�B	O�B	S�B	W
B	YB	YB	YB	YB	\)B	^5B	aHB	cTB	dZB	ffB	ffB	gmB	iyB	l�B	o�B	p�B	p�B	p�B	p�B	s�B	u�B	v�B	y�B	{�B	|�B	~�B	~�B	� B	�B	�B	� B	�B	�B	�B	�+B	�1B	�7B	�7B	�=B	�DB	�JB	�PB	�VB	�oB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�3B	�9B	�9B	�?B	�?B	�FB	�LB	�RB	�XB	�^B	�dB	�dB	�qB	�}B	��B	��B	B	B	ĜB	ĜB	ĜB	ĜB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�/B	�;B	�BB	�BB	�BB	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
1B
	7B

=B

=B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
JB
VB
\B
VB
\B
\B
VB
\B
bB
bB
hB
hB
hB
hB
hB
oB
oB
uB
uB
�B
�B
�B
)DB
;0222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB7LB7LB:^B?}BA�BI�BK�BL�BL�BK�BJ�BL�B]/B`BBbNBbNBe`Bn�Bp�Bm�Bl�Bq�Bp�Bo�Bo�By�B�oB��B��B��B�B�B�B�!BB�B��BB�BN�BZBdZBe`BffBffBhsBl�Bk�BjBl�Bt�Bz�B�B�B~�Bm�B]/BW
BM�B=qB7LB33B"�B��B�#B�B~�Be`BR�B>wB�BbBB
�fB
B
�VB
ffB
W
B
K�B
5?B
�B	�B	��B	��B	�B	��B	��B	�oB	�PB	�+B	{�B	r�B	k�B	_;B	VB	L�B	=qB	/B	%�B	�B	uB	B�B�TB��BÖB��B�XB�B��B��B��B�B�B�B��B��B�-B�'B�3B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B�B��B��B��B��B��B��B��B�B�B�LB�dB�qB��BĜBÖBŢBŢBŢBÖB��B�B�
B�/B�BB�HB�NB�`B�`B�yB�yB�yB�B�B�B�yB�B�B�B�B�yB�sB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B	B	%B	PB	\B	oB	uB	�B	�B	�B	�B	�B	"�B	%�B	'�B	'�B	+B	/B	1'B	33B	5?B	8RB	9XB	8RB	6FB	8RB	=qB	=qB	<jB	;dB	;dB	<jB	A�B	E�B	G�B	J�B	O�B	S�B	W
B	YB	YB	YB	YB	\)B	^5B	aHB	cTB	dZB	ffB	ffB	gmB	iyB	l�B	o�B	p�B	p�B	p�B	p�B	s�B	u�B	v�B	y�B	{�B	|�B	~�B	~�B	� B	�B	�B	� B	�B	�B	�B	�+B	�1B	�7B	�7B	�=B	�DB	�JB	�PB	�VB	�oB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�3B	�9B	�9B	�?B	�?B	�FB	�LB	�RB	�XB	�^B	�dB	�dB	�qB	�}B	��B	��B	B	B	ĜB	ĜB	ĜB	ĜB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�/B	�;B	�BB	�BB	�BB	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
1B
	7B

=B

=B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
JB
VB
\B
VB
\B
\B
VB
\B
bB
bB
hB
hB
hB
hB
hB
oB
oB
uB
uB
�B
�B
�B
)DB
;0222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190541                              AO  ARCAADJP                                                                    20181005190541    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190541  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190541  QCF$                G�O�G�O�G�O�8000            
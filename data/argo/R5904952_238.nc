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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190559  20181005190559  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��鑢�f1   @���q��@0���O�;�c��vȴ91   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�33A�33A�  A�  A�  B   B  B��B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC1�fC3�fC6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��3C�  C��3D y�D ��Dy�D��D� D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  Dy�D  D�fD  D� D  D� D  D�fD  D� D  D�fD  D� D  Dy�D  D� DfD�fDfD�fD  Dy�D  D� D  D� D  D� D��D� D  D� D��D� D��D� DfD�fD fD � D!  D!� D!��D"y�D#  D#�fD$  D$� D%  D%� D&  D&y�D&��D'� D(fD(� D)  D)� D)��D*y�D*��D+� D,fD,�fD-  D-� D-��D.y�D/  D/�fD0fD0� D1fD1� D1��D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?y�D@  D@� DA  DA� DB  DBy�DB��DC� DD  DD� DE  DE� DF  DF�fDGfDG�fDH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ�fDR  DRy�DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW�fDXfDX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^fD^� D_  D_� D`fD`� Da  Da� Da��Dby�Db��Dc� DdfDd� Dd��De� DffDf� Df��Dg� DhfDh�fDifDi�fDj  Dj�fDk  Dky�Dl  Dly�Dm  Dm� Dn  Dn�fDofDo� Do��Dpy�Dq  Dq�fDrfDr� Ds  Ds�fDt  Dty�Dt��Du� Dv  Dv� Dv��Dw� Dw��Dy��D�0RD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�=qA�A%�AE�Ae�A��\A��\A��\A�A�Aҏ\A�\A�\BG�B	G�B�HB�HB!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B���B���B��
B��
B���B���B���B�p�B���B���B���B���B���B��
Bȣ�Ḅ�B��
Bԣ�Bأ�Bܣ�B��B��B��B�p�B��B���B���B�p�C 8RCQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C08RC28RC48RC6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBk�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�Chk�CjQ�ClQ�CnQ�CpQ�Cr8RCtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�)C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�)C�(�C�5�C�(�C�5�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�)C�(�D D �DD�DD�{D{D�{D{D�{D{D�{D{D�{D�D�{D{D�{D	{D	�{D
{D
�{D{D�D{D��D{D�{D{D�{D{D��D{D�{D{D��D{D�{D{D�D{D�{D�D��D�D��D{D�D{D�{D{D�{D{D�{DD�{D{D�{DD�{DD�{D�D��D �D �{D!{D!�{D"D"�D#{D#��D${D$�{D%{D%�{D&{D&�D'D'�{D(�D(�{D){D)�{D*D*�D+D+�{D,�D,��D-{D-�{D.D.�D/{D/��D0�D0�{D1�D1�{D2D2�D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9�D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?D?�D@{D@�{DA{DA�{DB{DB�DCDC�{DD{DD�{DE{DE�{DF{DF��DG�DG��DH{DH�{DI{DI�{DJ{DJ�DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO�DO�{DP{DP�{DQ{DQ��DR{DR�DS{DS�{DT{DT�{DU{DU��DV{DV�{DW{DW��DX�DX�{DY{DY�{DZ{DZ�{D[{D[�{D\�D\�{D]{D]�{D^�D^�{D_{D_�{D`�D`�{Da{Da�{DbDb�DcDc�{Dd�Dd�{DeDe�{Df�Df�{DgDg�{Dh�Dh��Di�Di��Dj{Dj��Dk{Dk�Dl{Dl�Dm{Dm�{Dn{Dn��Do�Do�{DpDp�Dq{Dq��Dr�Dr�{Ds{Ds��Dt{Dt�DuDu�{Dv{Dv�{DwDw�{Dw�HDy�3D�:�D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�JA�bA��A� �A�$�A�$�A�&�A�&�A�$�A�(�A�(�A�(�A�(�A�(�A�+A�-A�-A�$�A� �A� �A�"�A�$�A�(�A�+A�5?A�1'A�7LA�/A��Aǝ�A�VA��A��A�%A���AǾwAǝ�A�x�A�VA� �A�A��A� �Aĉ7A�"�Aú^A��yA���A���A��hA�&�A�"�A���A�M�A��PA�9XA���A��uA�1A��A���A���A��+A���A�^5A�p�A�&�A�33A��A��mA���A�1'A���A�ĜA�S�A��9A�&�A��jA��jA�{A�v�A���A���A��A��\A���A�-A�JA���A��wA��A��A��TA�;dA��-A�  A���A�ȴA�-A�z�A��A���A�p�A��A���A�\)A�A~�A}7LA{�A{`BA{/AvZAo��Am`BAiG�Ae�A_\)A^JA\�AW��AW?}AT-AP��AL�AI�hAG`BAF�jAD�/AA33A>�A=�A:��A8M�A61'A4�`A4  A3��A2�A.��A.{A-��A, �A)�A(JA'�;A'��A&~�A%7LA$z�A#/A!��A!t�A �DA�A"�AXA^5A�`AoA�A�\A�wAhsAE�A9XA�wAK�A?}AZAƨA�`A
�/A
ZA	�-A	�A��A�wA��A$�Ap�A �AhsAp�At�A`BAK�A"�AbAhsA ��@��H@���@���@�{@���@�O�@�  @�V@��@�`B@��@�A�@�|�@�7@��@�+@��#@�Ĝ@�1@睲@�|�@�V@�r�@���@��@�7L@�I�@���@�O�@���@���@̃@��@ˍP@��y@ʧ�@���@�`B@�7L@ț�@��@���@�ƨ@�v�@őh@�Z@�z�@�bN@��
@�K�@�E�@���@�Q�@���@���@�G�@��@��@�K�@���@�-@�V@��w@��P@�|�@�C�@��@�S�@�t�@��P@�+@�o@��@�$�@���@�%@��;@���@�S�@��@�~�@�@�@�7L@�9X@��@���@�;d@��@��!@�V@��@�@�@���@��@�X@�%@� �@�l�@���@���@�5?@�J@��@��@���@���@��@��-@��#@���@��^@�`B@�Ĝ@��D@�9X@� �@�b@��;@�ƨ@�@���@�?}@�&�@��`@�z�@�bN@��D@���@��D@�  @��w@�C�@��\@�J@��@��T@��h@�X@��@��j@�Q�@��;@���@��@�I�@�j@��@�A�@��w@�C�@��H@�~�@�ff@�V@��#@��-@�p�@�V@�%@���@��9@��@�"�@���@�^5@���@�O�@���@���@�r�@� �@���@�dZ@�
=@��R@�n�@�M�@�-@���@��h@�?}@��@���@�A�@� �@�  @��@��@���@�~�@�-@��@�@���@���@�&�@��@��`@���@��j@�Z@��@��@��m@���@���@���@�;d@��R@�M�@�$�@�5?@�-@�{@��7@�`B@�O�@���@���@��9@��@��D@�bN@�9X@�ƨ@�l�@��@��R@���@��+@�=q@��@��@��@���@���@���@��-@�X@�%@�Ĝ@�Ĝ@���@�O�@�G�@�/@�&�@��@���@�bN@���@�A�@�1@�  @��m@���@�"�@��@��R@��+@�M�@�=q@�$�@�@��T@��h@�p�@�X@�7L@���@���@�j@��
@��@�|�@�o@��H@���@���@�~(@~� @lu�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�JA�bA��A� �A�$�A�$�A�&�A�&�A�$�A�(�A�(�A�(�A�(�A�(�A�+A�-A�-A�$�A� �A� �A�"�A�$�A�(�A�+A�5?A�1'A�7LA�/A��Aǝ�A�VA��A��A�%A���AǾwAǝ�A�x�A�VA� �A�A��A� �Aĉ7A�"�Aú^A��yA���A���A��hA�&�A�"�A���A�M�A��PA�9XA���A��uA�1A��A���A���A��+A���A�^5A�p�A�&�A�33A��A��mA���A�1'A���A�ĜA�S�A��9A�&�A��jA��jA�{A�v�A���A���A��A��\A���A�-A�JA���A��wA��A��A��TA�;dA��-A�  A���A�ȴA�-A�z�A��A���A�p�A��A���A�\)A�A~�A}7LA{�A{`BA{/AvZAo��Am`BAiG�Ae�A_\)A^JA\�AW��AW?}AT-AP��AL�AI�hAG`BAF�jAD�/AA33A>�A=�A:��A8M�A61'A4�`A4  A3��A2�A.��A.{A-��A, �A)�A(JA'�;A'��A&~�A%7LA$z�A#/A!��A!t�A �DA�A"�AXA^5A�`AoA�A�\A�wAhsAE�A9XA�wAK�A?}AZAƨA�`A
�/A
ZA	�-A	�A��A�wA��A$�Ap�A �AhsAp�At�A`BAK�A"�AbAhsA ��@��H@���@���@�{@���@�O�@�  @�V@��@�`B@��@�A�@�|�@�7@��@�+@��#@�Ĝ@�1@睲@�|�@�V@�r�@���@��@�7L@�I�@���@�O�@���@���@̃@��@ˍP@��y@ʧ�@���@�`B@�7L@ț�@��@���@�ƨ@�v�@őh@�Z@�z�@�bN@��
@�K�@�E�@���@�Q�@���@���@�G�@��@��@�K�@���@�-@�V@��w@��P@�|�@�C�@��@�S�@�t�@��P@�+@�o@��@�$�@���@�%@��;@���@�S�@��@�~�@�@�@�7L@�9X@��@���@�;d@��@��!@�V@��@�@�@���@��@�X@�%@� �@�l�@���@���@�5?@�J@��@��@���@���@��@��-@��#@���@��^@�`B@�Ĝ@��D@�9X@� �@�b@��;@�ƨ@�@���@�?}@�&�@��`@�z�@�bN@��D@���@��D@�  @��w@�C�@��\@�J@��@��T@��h@�X@��@��j@�Q�@��;@���@��@�I�@�j@��@�A�@��w@�C�@��H@�~�@�ff@�V@��#@��-@�p�@�V@�%@���@��9@��@�"�@���@�^5@���@�O�@���@���@�r�@� �@���@�dZ@�
=@��R@�n�@�M�@�-@���@��h@�?}@��@���@�A�@� �@�  @��@��@���@�~�@�-@��@�@���@���@�&�@��@��`@���@��j@�Z@��@��@��m@���@���@���@�;d@��R@�M�@�$�@�5?@�-@�{@��7@�`B@�O�@���@���@��9@��@��D@�bN@�9X@�ƨ@�l�@��@��R@���@��+@�=q@��@��@��@���@���@���@��-@�X@�%@�Ĝ@�Ĝ@���@�O�@�G�@�/@�&�@��@���@�bN@���@�A�@�1@�  @��m@���@�"�@��@��R@��+@�M�@�=q@�$�@�@��T@��h@�p�@�X@�7L@���@���@�j@��
@��@�|�@�o@��H@���@���@�~(@~� @lu�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BDBDBDBDBDBDBDBDB
=BDBDBDBDBJBJBDBDB
=B	7B
=BDBbB�B�B �B"�B�B1'B�uB	9XB	x�B	�+B	�DB	�VB	�hB	��B	�B	�B	ɺB
uB
D�B
��B
��B
��BBhB33BaHB�{B��B�)B�/B�;B�BBPB�B%�BB�BD�BC�B@�BC�BQ�BP�BQ�BN�BO�BM�BB�B>wB;dB9XB+B�B	7B��B�B�;B��BĜB�-B��B�7BjBP�BF�B2-BJB
��B
�B
��B
�3B
��B
�hB
�%B
p�B
XB
N�B
F�B
A�B
=qB
8RB
.B
"�B
�B
%B	��B	�yB	�BB	�B	��B	�3B	�%B	q�B	YB	B�B	%�B	�B	{B��B�B�BÖB�LB�B��B��B��B�uB�bB�bB�uB�{B��B�{B��B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�FB�dBÖB�}B�dB�9B�B�3B�3B�-B�3B�^B�dB�qB�qB��B�}B�wB�qB�dB�^B�XB�RB�XB�LB�FB�3B�B�FB�XB�^B�dB�jBBȴB��BɺBɺB��BȴBÖBÖB��B��BÖBǮBȴBȴB��B��B��B��B��B��B��B�B��B��B�B�)B�5B�;B�BB�5B	
=B	\B	{B	�B	�B	"�B	(�B	-B	/B	2-B	5?B	5?B	6FB	8RB	:^B	=qB	B�B	C�B	B�B	I�B	K�B	O�B	P�B	L�B	F�B	K�B	T�B	YB	ZB	ZB	[#B	[#B	[#B	[#B	]/B	]/B	`BB	bNB	dZB	ffB	jB	m�B	s�B	u�B	y�B	|�B	� B	�B	�+B	�=B	�PB	�VB	�VB	�\B	�hB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�FB	�XB	�dB	�dB	�jB	�jB	�wB	�wB	�}B	��B	��B	B	ƨB	ƨB	ŢB	ƨB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�BB	�HB	�TB	�`B	�mB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
%B
B
%B
%B
+B
+B
1B
1B
+B
+B
1B
1B
1B
	7B
	7B

=B
DB
JB
JB
JB
JB
JB
JB
DB
JB
JB
DB
PB
VB
\B
\B
\B
\B
\B
VB
\B
\B
\B
\B
bB
uB
uB
uB
{B
{B
�B
{B
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
2aB
@�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 BDBDBDBDBDBDBDBDB
=BDBDBDBDBJBJBDBDB
=B	7B
=BDBbB�B�B �B"�B�B1'B�uB	9XB	x�B	�+B	�DB	�VB	�hB	��B	�B	�B	ɺB
uB
D�B
��B
��B
��BBhB33BaHB�{B��B�)B�/B�;B�BBPB�B%�BB�BD�BC�B@�BC�BQ�BP�BQ�BN�BO�BM�BB�B>wB;dB9XB+B�B	7B��B�B�;B��BĜB�-B��B�7BjBP�BF�B2-BJB
��B
�B
��B
�3B
��B
�hB
�%B
p�B
XB
N�B
F�B
A�B
=qB
8RB
.B
"�B
�B
%B	��B	�yB	�BB	�B	��B	�3B	�%B	q�B	YB	B�B	%�B	�B	{B��B�B�BÖB�LB�B��B��B��B�uB�bB�bB�uB�{B��B�{B��B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�FB�dBÖB�}B�dB�9B�B�3B�3B�-B�3B�^B�dB�qB�qB��B�}B�wB�qB�dB�^B�XB�RB�XB�LB�FB�3B�B�FB�XB�^B�dB�jBBȴB��BɺBɺB��BȴBÖBÖB��B��BÖBǮBȴBȴB��B��B��B��B��B��B��B�B��B��B�B�)B�5B�;B�BB�5B	
=B	\B	{B	�B	�B	"�B	(�B	-B	/B	2-B	5?B	5?B	6FB	8RB	:^B	=qB	B�B	C�B	B�B	I�B	K�B	O�B	P�B	L�B	F�B	K�B	T�B	YB	ZB	ZB	[#B	[#B	[#B	[#B	]/B	]/B	`BB	bNB	dZB	ffB	jB	m�B	s�B	u�B	y�B	|�B	� B	�B	�+B	�=B	�PB	�VB	�VB	�\B	�hB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�FB	�XB	�dB	�dB	�jB	�jB	�wB	�wB	�}B	��B	��B	B	ƨB	ƨB	ŢB	ƨB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�BB	�HB	�TB	�`B	�mB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
%B
B
%B
%B
+B
+B
1B
1B
+B
+B
1B
1B
1B
	7B
	7B

=B
DB
JB
JB
JB
JB
JB
JB
DB
JB
JB
DB
PB
VB
\B
\B
\B
\B
\B
VB
\B
\B
\B
\B
bB
uB
uB
uB
{B
{B
�B
{B
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
2aB
@�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190559                              AO  ARCAADJP                                                                    20181005190559    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190559  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190559  QCF$                G�O�G�O�G�O�8000            
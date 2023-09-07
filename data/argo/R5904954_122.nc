CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:16Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191716  20181005191716  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               zA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�����Q�1   @���{Bq�@4��/���dd1&�x�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      zA   A   B   @9��@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���A�33B��B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC�fC�fC"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<�C>�C@  CB  CD�CF  CH  CJ  CL  CN  CP�CR  CT  CV  CW�fCZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn�Cp  Cq�fCs�fCv  Cx  Cy�fC{�fC}�fC�  C��C�  C�  C�  C��3C��3C��3C��3C�  C��3C��3C��3C��fC��fC��3C�  C��C�  C�  C��3C��3C��3C�  C�  C��3C��3C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C��3C��3C�  C��C��C�  C�  C��C��C��C��C��C�  C��3C�  C��C��C��3C�  C��C��C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��C�  C��fC�  C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C��C��3C�  C�  C��C��3C�  C��C��C�  C��3C��3C�  C��C��3C�  C�  C��C�  C�  C�  C��C��D   D y�D ��Dy�D  D� D��D� DfD� D  D�fD��Dy�DfD�fDfD� D��D	� D	��D
� DfD� D  D�fD  D� D  Dy�D��D�fD  D� D  D� D��D�fD  Dy�D  D� D  D�fDfD�fD  Dy�D  D� D  D� D��Dy�D  D� D��D�fD  D� D��D� D  D� D   D � D ��D!�fD"fD"�fD#fD#�fD/y�D/��D0�fD1�D1�fD2  D2�fD3  D3� D4  D4� D5  D5�fD6  D6y�D7  D7� D8  D8�fD9  D9� D:fD:� D;  D;�fD<fD<�fD=  D=y�D=��D>y�D>�3D?y�D@fD@�fDAfDA� DA��DBy�DC  DC� DD  DDy�DD��DE�fDFfDF� DG  DGs3DG�3DHy�DIfDI�fDJfDJ�fDK  DK� DLfDL�fDMfDM� DM��DN� DN��DOy�DP  DP�fDP��DQs3DQ��DR� DSfDS� DT  DT� DT��DUy�DVfDV� DW  DW� DW��DXy�DYfDY�fDZfDZ�fD[  D[y�D\  D\� D]  D]� D^  D^y�D^��D_�fD`  D`�fDafDa�fDb  Dby�Dc  Dcy�DdfDd�fDe  De� DffDf� Df��Dg�fDhfDh�fDh��Diy�Dj  Dj� DkfDk� Dl  Dl�fDm  Dm� Dn  Dny�Dn��Do� Dp  Dp�fDqfDq� Dq�3Dry�Dr��Ds� Ds��Dts3Dt��Du�fDv  Dvs3Dw  Dw� Dw� Dy�{D�*�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @N{@�p�@�=qA�A%�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\)B �HB�HBG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQ�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B�p�B���B���B���B���B���B���B���B���B���B���B���B�p�Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B�p�B�p�B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C8RC8RC 8RC"Q�C$Q�C&k�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6k�C8Q�C:Q�C<k�C>k�C@Q�CBQ�CDk�CFQ�CHQ�CJQ�CLQ�CNQ�CPk�CRQ�CTQ�CVQ�CX8RCZQ�C\Q�C^Q�C`Q�Cb8RCdQ�CfQ�ChQ�CjQ�ClQ�Cnk�CpQ�Cr8RCt8RCvQ�CxQ�Cz8RC|8RC~8RC�(�C�5�C�(�C�(�C�(�C�)C�)C�)C�)C�(�C�)C�)C�)C�\C�\C�)C�(�C�5�C�(�C�(�C�)C�)C�)C�(�C�(�C�)C�)C�(�C�5�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�)C�(�C�(�C�)C�)C�)C�(�C�5�C�5�C�(�C�(�C�5�C�5�C�5�C�B�C�5�C�(�C�)C�(�C�5�C�5�C�)C�(�C�5�C�5�C�)C�)C�)C�)C�(�C�(�C�)C�)C�)C�)C�5�C�(�C�\C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�5�C�)C�(�C�(�C�5�C�)C�(�C�5�C�5�C�(�C�)C�)C�(�C�5�C�)C�(�C�(�C�5�C�(�C�(�C�(�C�5�C�5�D {D �DD�D{D�{DD�{D�D�{D{D��DD�D�D��D�D�{D	D	�{D
D
�{D�D�{D{D��D{D�{D{D�DD��D{D�{D{D�{DD��D{D�D{D�{D{D��D�D��D{D�D{D�{D{D�{DD�D{D�{DD��D{D�{DD�{D{D�{D {D �{D!D!��D"�D"��D#�D#��D/�D0D0��D1!HD1��D2{D2��D3{D3�{D4{D4�{D5{D5��D6{D6�D7{D7�{D8{D8��D9{D9�{D:�D:�{D;{D;��D<�D<��D={D=�D>D>�D?�D?�D@�D@��DA�DA�{DBDB�DC{DC�{DD{DD�DEDE��DF�DF�{DG{DG��DH�DH�DI�DI��DJ�DJ��DK{DK�{DL�DL��DM�DM�{DNDN�{DODO�DP{DP��DQDQ��DRDR�{DS�DS�{DT{DT�{DUDU�DV�DV�{DW{DW�{DXDX�DY�DY��DZ�DZ��D[{D[�D\{D\�{D]{D]�{D^{D^�D_D_��D`{D`��Da�Da��Db{Db�Dc{Dc�Dd�Dd��De{De�{Df�Df�{DgDg��Dh�Dh��DiDi�Dj{Dj�{Dk�Dk�{Dl{Dl��Dm{Dm�{Dn{Dn�DoDo�{Dp{Dp��Dq�Dq�{Dr�Dr�DsDs�{DtDt��DuDu��Dv{Dv��Dw{Dw�{Dw�{Dy��D�51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AܬAܬAܮAܮAܬAܬAܮAܮAܬAܩ�Aܣ�Aܡ�Aܗ�A�\)A�G�A� �A��A��TA�JAЗ�A��#A�t�AͼjA�G�A���A�ĜA�z�A��A��`A�A�A�Q�A���A�1'A�E�A��A�1'A��A�|�A���A���A���A�n�A�-A��PA���A���A���A�O�A�^5A��A���A��#A�S�A��A��/A���A�$�A�G�A��\A�1A�=qA�|�A��HA�^5A�bA���A��uA�+A��TA��A���A��A���A���A�5?A�?}A�"�A��jA�|�A��A�"�A~(�AzM�Ax�uAwS�Au��At-Ar1'AoXAnE�Amp�Ak�^Aj�HAj5?Ai/AhJAgAd�DAa��A_�#A\��A\bAZ��AY�AX��AX�AV��AS��AQ�^APZAOXAM��AJ�+AIƨAI�AG��AE��AD�DA@�DA>��A=�
A=`BA<�\A8��A7A7oA6��A5�TA5�A45?A2�A1��A1S�A0^5A/�A. �A-�A,��A*��A)C�A(�`A(A�A&��A%��A%|�A%\)A#A"5?A"(�A"bA!l�A n�A�A�yA�A��AG�A�mA�/AA�A�mA�hA�yAK�A&�A~�AA�A�mA�
A��AƨAt�AXA�yA��A+A��AZA�A;dA��A-A�A�FAt�A�AȴA1'AK�A	�A^5A|�A7LAffA�A�PAt�A�A�/A�hA��A�AoA ��@�o@�X@���@��@��y@�x�@�%@���@��`@��@�r�@�1@�C�@��#@�  @�dZ@��@��T@�`B@���@�`B@�l�@��@�x�@�ff@�V@ش9@�E�@ԃ@��@Ѻ^@мj@�^5@��;@�9X@͉7@�hs@�X@�E�@Ο�@�z�@�ȴ@��/@ǍP@���@��@š�@ŉ7@���@\@�Q�@�^5@���@�1@��;@��T@���@�"�@�`B@���@��#@�33@���@��D@� �@��H@�@�$�@�-@���@���@� �@��H@���@���@�Z@���@��;@���@�1'@�Z@��`@��@�@��@�ƨ@��u@�z�@��m@�ff@��y@��h@�O�@��-@���@��\@���@���@��^@�~�@�p�@�;d@�V@�M�@�v�@��@�t�@�@��!@�~�@�/@�%@��@���@�7L@���@��+@�%@�M�@���@�o@��m@��@��-@�7L@�(�@���@���@�&�@�%@��-@��@��@�ȴ@�M�@�O�@��@�&�@��j@� �@��@��@�1@���@�C�@�n�@�=q@��T@���@��@��@���@�x�@��@�bN@�ƨ@��P@�33@�
=@�ȴ@���@�V@��+@�^5@�-@��h@�V@�Ĝ@��P@��@��P@�@��+@�@��7@�7L@�V@�%@���@��/@���@�z�@�A�@� �@�  @�t�@�C�@��@�ȴ@��R@��\@�5?@�@�@�7L@���@�j@��@�;d@�
=@�@���@��@��@��H@�n�@��#@�/@���@�Z@�  @��F@�V@�@�X@��/@���@���@���@���@�Z@�(�@��@��@���@�S�@�o@���@�E�@�G�@��`@�1'@��@�t�@�C�@���@��@���@���@��-@���@�G�@���@�V@��`@��u@�j@�A�@�b@��@l�@;d@~��@~ȴ@~��@~@}�@}�T@}��@}?}@|�j@{��@{33@z�@y�^@y�#@z�\@{"�@{ƨ@z��@j_�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AܬAܬAܮAܮAܬAܬAܮAܮAܬAܩ�Aܣ�Aܡ�Aܗ�A�\)A�G�A� �A��A��TA�JAЗ�A��#A�t�AͼjA�G�A���A�ĜA�z�A��A��`A�A�A�Q�A���A�1'A�E�A��A�1'A��A�|�A���A���A���A�n�A�-A��PA���A���A���A�O�A�^5A��A���A��#A�S�A��A��/A���A�$�A�G�A��\A�1A�=qA�|�A��HA�^5A�bA���A��uA�+A��TA��A���A��A���A���A�5?A�?}A�"�A��jA�|�A��A�"�A~(�AzM�Ax�uAwS�Au��At-Ar1'AoXAnE�Amp�Ak�^Aj�HAj5?Ai/AhJAgAd�DAa��A_�#A\��A\bAZ��AY�AX��AX�AV��AS��AQ�^APZAOXAM��AJ�+AIƨAI�AG��AE��AD�DA@�DA>��A=�
A=`BA<�\A8��A7A7oA6��A5�TA5�A45?A2�A1��A1S�A0^5A/�A. �A-�A,��A*��A)C�A(�`A(A�A&��A%��A%|�A%\)A#A"5?A"(�A"bA!l�A n�A�A�yA�A��AG�A�mA�/AA�A�mA�hA�yAK�A&�A~�AA�A�mA�
A��AƨAt�AXA�yA��A+A��AZA�A;dA��A-A�A�FAt�A�AȴA1'AK�A	�A^5A|�A7LAffA�A�PAt�A�A�/A�hA��A�AoA ��@�o@�X@���@��@��y@�x�@�%@���@��`@��@�r�@�1@�C�@��#@�  @�dZ@��@��T@�`B@���@�`B@�l�@��@�x�@�ff@�V@ش9@�E�@ԃ@��@Ѻ^@мj@�^5@��;@�9X@͉7@�hs@�X@�E�@Ο�@�z�@�ȴ@��/@ǍP@���@��@š�@ŉ7@���@\@�Q�@�^5@���@�1@��;@��T@���@�"�@�`B@���@��#@�33@���@��D@� �@��H@�@�$�@�-@���@���@� �@��H@���@���@�Z@���@��;@���@�1'@�Z@��`@��@�@��@�ƨ@��u@�z�@��m@�ff@��y@��h@�O�@��-@���@��\@���@���@��^@�~�@�p�@�;d@�V@�M�@�v�@��@�t�@�@��!@�~�@�/@�%@��@���@�7L@���@��+@�%@�M�@���@�o@��m@��@��-@�7L@�(�@���@���@�&�@�%@��-@��@��@�ȴ@�M�@�O�@��@�&�@��j@� �@��@��@�1@���@�C�@�n�@�=q@��T@���@��@��@���@�x�@��@�bN@�ƨ@��P@�33@�
=@�ȴ@���@�V@��+@�^5@�-@��h@�V@�Ĝ@��P@��@��P@�@��+@�@��7@�7L@�V@�%@���@��/@���@�z�@�A�@� �@�  @�t�@�C�@��@�ȴ@��R@��\@�5?@�@�@�7L@���@�j@��@�;d@�
=@�@���@��@��@��H@�n�@��#@�/@���@�Z@�  @��F@�V@�@�X@��/@���@���@���@���@�Z@�(�@��@��@���@�S�@�o@���@�E�@�G�@��`@�1'@��@�t�@�C�@���@��@���@���@��-@���@�G�@���@�V@��`@��u@�j@�A�@�b@��@l�@;d@~��@~ȴ@~��@~@}�@}�T@}��@}?}@|�j@{��@{33@z�@y�^@y�#@z�\@{"�@{ƨ@z��@j_�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B=qB=qB=qB=qB=qB=qB=qB>wB?}B?}B@�BA�BC�BP�BYBYB5?B(�B(�B(�B&�B$�B(�BH�BO�BVB]/Be`Bt�B|�B�B�%B�1B�DB�=B�=B�PB�VB�VB�DB�+B�B� B}�Bz�Bs�BhsB^5BT�BF�B33B�BVB�B��B�}B�3B��B�1B}�Bt�BiyBaHBZBVBQ�BM�BG�BC�B<jB"�B{B1B
��B
�B
ÖB
��B
�%B
r�B
YB
E�B
6FB
�B
bB
+B	��B	�B	�ZB	��B	ɺB	ÖB	�RB	�-B	�B	��B	��B	��B	�7B	z�B	p�B	cTB	^5B	XB	Q�B	L�B	H�B	A�B	2-B	'�B	�B	�B	\B	B��B��B�B�mB�HB��B��BɺBǮBÖB�dB�RB�FB�?B�-B�!B�B�B��B��B��B��B��B��B��B��B�uB�oB�bB�VB�PB�JB�=B�=B�7B�7B�1B�+B�+B�B�B�B�B�B�B� B}�B|�B{�Bz�Bt�Bq�Bo�Bo�Bp�Bp�Bp�Bp�Br�Bq�Bs�Bx�Bx�Bw�Bw�Bw�Bw�Bx�Bx�By�Bz�Bz�Bz�Bz�Bz�Bz�B{�Bu�Bs�Bq�Bn�Bk�BiyBo�Bx�Bx�Br�Bo�Bn�BhsBhsBaHB`BBaHBYBT�BR�BR�BT�BT�BVBXBZBZB\)BYBP�B[#B^5BbNBe`Bn�Bo�Bn�Bn�BiyBXBYBVBQ�BI�BK�BI�BB�B=qBB�BN�BQ�BVBbNBo�Bm�Bo�Bm�Bk�BiyBffBe`BdZBcTB`BB\)BXBXB\)BaHBt�B{�B~�B{�Bt�BffB_;B[#B[#BbNBgmBhsBl�Bp�Bx�B�B�7B��B�B�B�9B�jB�qB�XB�^B�jB��B�5B�fB�B��B�B�B�jB�FB�jB��B��B�B�NB�BB�5B�NB�sB�B		7B	�B	�B	�B	�B	{B	JB	VB	uB	-B	>wB	B�B	F�B	XB	bNB	bNB	YB	T�B	M�B	S�B	r�B	{�A�n�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�9B	�9B	�?B	�LB	�LB	�LB	�RB	�XB	�dB	�jB	�jB	�qB	�qB	��B	B	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�/B	�;B	�BB	�BB	�HB	�TB	�ZB	�`B	�`B	�mB	�mB	�mB	�mB	�fB	�sB	�B	�B	�B	�B	�B	�yB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
1B
�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B=qB=qB=qB=qB=qB=qB=qB>wB?}B?}B@�BA�BC�BP�BYBYB5?B(�B(�B(�B&�B$�B(�BH�BO�BVB]/Be`Bt�B|�B�B�%B�1B�DB�=B�=B�PB�VB�VB�DB�+B�B� B}�Bz�Bs�BhsB^5BT�BF�B33B�BVB�B��B�}B�3B��B�1B}�Bt�BiyBaHBZBVBQ�BM�BG�BC�B<jB"�B{B1B
��B
�B
ÖB
��B
�%B
r�B
YB
E�B
6FB
�B
bB
+B	��B	�B	�ZB	��B	ɺB	ÖB	�RB	�-B	�B	��B	��B	��B	�7B	z�B	p�B	cTB	^5B	XB	Q�B	L�B	H�B	A�B	2-B	'�B	�B	�B	\B	B��B��B�B�mB�HB��B��BɺBǮBÖB�dB�RB�FB�?B�-B�!B�B�B��B��B��B��B��B��B��B��B�uB�oB�bB�VB�PB�JB�=B�=B�7B�7B�1B�+B�+B�B�B�B�B�B�B� B}�B|�B{�Bz�Bt�Bq�Bo�Bo�Bp�Bp�Bp�Bp�Br�Bq�Bs�Bx�Bx�Bw�Bw�Bw�Bw�Bx�Bx�By�Bz�Bz�Bz�Bz�Bz�Bz�B{�Bu�Bs�Bq�Bn�Bk�BiyBo�Bx�Bx�Br�Bo�Bn�BhsBhsBaHB`BBaHBYBT�BR�BR�BT�BT�BVBXBZBZB\)BYBP�B[#B^5BbNBe`Bn�Bo�Bn�Bn�BiyBXBYBVBQ�BI�BK�BI�BB�B=qBB�BN�BQ�BVBbNBo�Bm�Bo�Bm�Bk�BiyBffBe`BdZBcTB`BB\)BXBXB\)BaHBt�B{�B~�B{�Bt�BffB_;B[#B[#BbNBgmBhsBl�Bp�Bx�B�B�7B��B�B�B�9B�jB�qB�XB�^B�jB��B�5B�fB�B��B�B�B�jB�FB�jB��B��B�B�NB�BB�5B�NB�sB�B		7B	�B	�B	�B	�B	{B	JB	VB	uB	-B	>wB	B�B	F�B	XB	bNB	bNB	YB	T�B	M�B	S�B	r�B	{�A�n�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�9B	�9B	�?B	�LB	�LB	�LB	�RB	�XB	�dB	�jB	�jB	�qB	�qB	��B	B	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�/B	�;B	�BB	�BB	�HB	�TB	�ZB	�`B	�`B	�mB	�mB	�mB	�mB	�fB	�sB	�B	�B	�B	�B	�B	�yB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
1B
�B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191716                              AO  ARCAADJP                                                                    20181005191716    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191716  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191716  QCF$                G�O�G�O�G�O�8000            
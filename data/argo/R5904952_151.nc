CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:39Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190539  20181005190539  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)��B�1   @��*��@0���-V�c������1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @y��@�  A   A   A@  A`  A�  A�  A���A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  BpffBxffB�  B�  B�  B�  B�  B���B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�33B�33B�33B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C��3C��3C��3C��3C�  C�  C��3C�  C��C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C��3C�  C�  D   D y�D  D� DfD�fD  D�fD  Dy�D��D� D  D� D��Dy�D��D� D	  D	� D	��D
� D  D� D  D�fDfD�fD  D� D  D� DfD�fD  D� D��D� DfD� D��Dy�D��D� D  D� D  D� D  D� D  D� DfD� D  D�fD  D� D  D� DfD� D  D� D��D � D!  D!y�D"  D"�fD#  D#� D$  D$y�D$��D%� D&  D&� D'  D'y�D'��D(y�D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0fD0� D1  D1y�D2  D2� D3  D3�fD4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:fD:� D:��D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fD@��DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG�fDHfDH� DI  DI� DJfDJ�fDK  DK� DL  DLy�DM  DM� DM��DN� DO  DO�fDP  DPy�DP��DQ� DR  DRy�DS  DS�fDTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DYy�DZ  DZ� D[  D[�fD\  D\� D]  D]� D^  D^� D_  D_y�D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dhy�Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw�fDw�fDy��D�33D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
>@�=qA�A%�AE�Ae�A��\A��\A�\)A�\)A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�B`�HBiG�Bq�By�B���B���B���B���B���B�p�B���B�p�B���B���B��
B���B���B���B���B���B��
B��
B��
Ḅ�B��
B��
Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�Ck�CQ�CQ�CQ�C
Q�C8RCQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2k�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^8RC`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�Cpk�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�)C�)C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�5�C�(�C�(�C�)C�)C�)C�)C�(�C�(�C�)C�(�C�5�C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�)C�)C�(�C�(�D {D �D{D�{D�D��D{D��D{D�DD�{D{D�{DD�DD�{D	{D	�{D
D
�{D{D�{D{D��D�D��D{D�{D{D�{D�D��D{D�{DD�{D�D�{DD�DD�{D{D�{D{D�{D{D�{D{D�{D�D�{D{D��D{D�{D{D�{D�D�{D{D�{D D �{D!{D!�D"{D"��D#{D#�{D${D$�D%D%�{D&{D&�{D'{D'�D(D(�D){D)�{D*{D*�{D+{D+�{D,{D,��D-{D-�{D.{D.�{D/{D/�{D0�D0�{D1{D1�D2{D2�{D3{D3��D4�D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9D9�{D:�D:�{D;D;�D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@��DADA�{DB{DB�{DC{DC�{DD�DD�{DE{DE�{DF{DF�{DG{DG��DH�DH�{DI{DI�{DJ�DJ��DK{DK�{DL{DL�DM{DM�{DNDN�{DO{DO��DP{DP�DQDQ�{DR{DR�DS{DS��DT�DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DYDY�DZ{DZ�{D[{D[��D\{D\�{D]{D]�{D^{D^�{D_{D_�D`{D`�{Da{Da��Db{Db�{Dc{Dc�{Dd{Dd�{De�De�{Df{Df�{Dg{Dg�{Dh{Dh�Di{Di��Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�Dn{Dn��Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw�Dw��Dw��Dy�\D�=pD�ʏ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S�A�XA�bNA�dZA�hsA�hsA�jA�l�A�n�A�jA�l�A�hsA�hsA�VA�G�A�?}A�7LA�5?A�7LA�33A�7LA�5?A�5?A�33A�1'A�1'A�/A�1'A�33A�33A�5?A�7LA�7LA�5?A�5?A�33A�33A�33A�5?A�5?A�5?A�5?A�5?A�33A�33A�33A�5?A�7LA�7LA�9XA�9XA�7LA�7LA�7LA�7LA�/A�A� �A�r�A���A�l�A�bNAȴ9A�hsA��`A�A�5?A�1A�+A��A¡�A��A�v�A��A�A�O�A���A��A�%A��A�z�A�Q�A��A�%A��\A�9XA���A��hA���A��-A�p�A��A�A�/A�-A��`A�I�A�ZA���A�A�/A��9A���A���A���A��A��DA��!A�S�A�A�A�oA���A�G�A�"�A|ffAxz�AtAo�AljAj�Ai�FAgl�AeXAbȴA`9XA\��AV9XATAR9XAPAM7LAJ�9AI�AG�AD�!ACS�AB1A?p�A<�/A;?}A:��A8�DA6��A6�A5%A3�TA1��A/t�A+�wA*r�A(��A'�TA'?}A&�A#�hA ��A 1'AA33A\)A �A�hA�9A�AK�A^5A;dA�DA�FA��A�FAA�At�A��AdZAXA�`Ap�A~�AI�A
��A��A�;A\)A��AG�A��An�A��A��A  A��A�A  �A =qA Z@��-@�b@���@�$�@��^@� �@��\@�^5@�{@�9X@�@��@��T@�9@�A�@띲@��y@��/@���@� �@��y@�Z@�F@�dZ@��@�-@���@��m@ߝ�@�t�@�ȴ@�x�@ܼj@�1'@�l�@�;d@�@ڗ�@؋D@�ȴ@�n�@ղ-@�A�@�dZ@�"�@�ff@�&�@�bN@�Q�@ϥ�@�@�~�@���@�@͉7@�O�@̃@�Q�@���@�@ʧ�@ʇ+@��@ʸR@�~�@�5?@��T@�x�@Ȭ@�  @�+@ź^@�{@�O�@ě�@�33@�"�@�33@��@��@�@�~�@�n�@�{@���@���@�x�@�hs@�O�@��@��/@�Z@��@�I�@���@�C�@�5?@��-@�J@�v�@�V@��@�1@�\)@�"�@�ff@��@�x�@��@��@���@��/@���@��j@���@��D@�9X@��w@�ƨ@�  @��@��@�M�@�$�@��@���@�x�@���@���@�r�@��@���@��F@�S�@��@��H@�@���@��#@��#@��@���@�I�@��@��m@�ƨ@���@��H@�5?@���@�p�@�G�@�&�@�%@���@��`@�bN@��
@��P@�K�@���@�=q@��@��#@�7L@���@��/@�z�@�9X@�1@���@���@�+@��@��+@�5?@���@�&�@���@�Ĝ@�bN@�1@��;@��P@�+@��@�ȴ@���@��+@���@��^@���@��7@��9@�1'@� �@���@��@�K�@�o@��y@��!@�V@���@��-@��@�V@���@�9X@��w@��P@�\)@��@��R@��\@�E�@�{@��^@��h@�?}@��/@�Ĝ@���@�A�@�1@��w@�\)@��@��R@���@��+@�ff@�$�@��^@��7@�p�@�?}@�/@��`@��j@���@�j@�1'@��
@���@�|�@�K�@��H@��!@��+@�^5@��@��h@�%@���@���@�I�@�  @��@��P@�l�@�\)@�K�@�ȴ@�$�@���@���@��h@��7@�?}@�&�@�&�@�V@���@��9@��D@�Q�@�A�@�(�@� �@�1@��w@��@�"�@��y@��!@�^5@�$�@���@��#@���@���@�?}@�%@���@���@���@��/@��/@���@uJ�@a�N111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�S�A�XA�bNA�dZA�hsA�hsA�jA�l�A�n�A�jA�l�A�hsA�hsA�VA�G�A�?}A�7LA�5?A�7LA�33A�7LA�5?A�5?A�33A�1'A�1'A�/A�1'A�33A�33A�5?A�7LA�7LA�5?A�5?A�33A�33A�33A�5?A�5?A�5?A�5?A�5?A�33A�33A�33A�5?A�7LA�7LA�9XA�9XA�7LA�7LA�7LA�7LA�/A�A� �A�r�A���A�l�A�bNAȴ9A�hsA��`A�A�5?A�1A�+A��A¡�A��A�v�A��A�A�O�A���A��A�%A��A�z�A�Q�A��A�%A��\A�9XA���A��hA���A��-A�p�A��A�A�/A�-A��`A�I�A�ZA���A�A�/A��9A���A���A���A��A��DA��!A�S�A�A�A�oA���A�G�A�"�A|ffAxz�AtAo�AljAj�Ai�FAgl�AeXAbȴA`9XA\��AV9XATAR9XAPAM7LAJ�9AI�AG�AD�!ACS�AB1A?p�A<�/A;?}A:��A8�DA6��A6�A5%A3�TA1��A/t�A+�wA*r�A(��A'�TA'?}A&�A#�hA ��A 1'AA33A\)A �A�hA�9A�AK�A^5A;dA�DA�FA��A�FAA�At�A��AdZAXA�`Ap�A~�AI�A
��A��A�;A\)A��AG�A��An�A��A��A  A��A�A  �A =qA Z@��-@�b@���@�$�@��^@� �@��\@�^5@�{@�9X@�@��@��T@�9@�A�@띲@��y@��/@���@� �@��y@�Z@�F@�dZ@��@�-@���@��m@ߝ�@�t�@�ȴ@�x�@ܼj@�1'@�l�@�;d@�@ڗ�@؋D@�ȴ@�n�@ղ-@�A�@�dZ@�"�@�ff@�&�@�bN@�Q�@ϥ�@�@�~�@���@�@͉7@�O�@̃@�Q�@���@�@ʧ�@ʇ+@��@ʸR@�~�@�5?@��T@�x�@Ȭ@�  @�+@ź^@�{@�O�@ě�@�33@�"�@�33@��@��@�@�~�@�n�@�{@���@���@�x�@�hs@�O�@��@��/@�Z@��@�I�@���@�C�@�5?@��-@�J@�v�@�V@��@�1@�\)@�"�@�ff@��@�x�@��@��@���@��/@���@��j@���@��D@�9X@��w@�ƨ@�  @��@��@�M�@�$�@��@���@�x�@���@���@�r�@��@���@��F@�S�@��@��H@�@���@��#@��#@��@���@�I�@��@��m@�ƨ@���@��H@�5?@���@�p�@�G�@�&�@�%@���@��`@�bN@��
@��P@�K�@���@�=q@��@��#@�7L@���@��/@�z�@�9X@�1@���@���@�+@��@��+@�5?@���@�&�@���@�Ĝ@�bN@�1@��;@��P@�+@��@�ȴ@���@��+@���@��^@���@��7@��9@�1'@� �@���@��@�K�@�o@��y@��!@�V@���@��-@��@�V@���@�9X@��w@��P@�\)@��@��R@��\@�E�@�{@��^@��h@�?}@��/@�Ĝ@���@�A�@�1@��w@�\)@��@��R@���@��+@�ff@�$�@��^@��7@�p�@�?}@�/@��`@��j@���@�j@�1'@��
@���@�|�@�K�@��H@��!@��+@�^5@��@��h@�%@���@���@�I�@�  @��@��P@�l�@�\)@�K�@�ȴ@�$�@���@���@��h@��7@�?}@�&�@�&�@�V@���@��9@��D@�Q�@�A�@�(�@� �@�1@��w@��@�"�@��y@��!@�^5@�$�@���@��#@���@���@�?}@�%@���@���@���@��/@��/@���@uJ�@a�N111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bv�Bv�Bv�Bu�Bv�Bv�Bw�Bw�Bw�Bv�Bw�Bw�Bw�Bw�Bw�Bx�Bx�Bx�By�Bx�By�By�By�Bx�Bx�Bx�Bx�By�By�By�By�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�B{�B{�B{�Bz�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B~�B�B�=B��B�B�/B�B�B��B	7B{B�B�B.B;dBN�BbNBl�Bp�Bq�Br�Bq�Bq�Bt�Bp�Bk�BffBbNB_;B]/BYBR�BH�B?}B49B2-B0!B)�B�BB�`BƨB�9B��B��Bx�BW
BI�B8RBPB
�B
��B
jB
=qB
"�B
hB
  B	�B	��B	�B	�DB	n�B	aHB	`BB	\)B	P�B	@�B	/B	�B	{B	B��B�B�sB�TB�;B�)B�B��B��B��BǮBB��B�wB�^B�RB�LB�?B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�B�B��B��B��B��B��B�-B�FB�RB�dB�jBÖBǮB��B��B�
B�mB�B�yB�sB�mB�fB�mB�mB�B�B��B��B�B�yB�`B�TB�NB�NB�ZB�`B�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	B	B��B��B��B	B	DB	bB	{B	uB	hB	bB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	(�B	/B	9XB	=qB	>wB	?}B	@�B	A�B	D�B	D�B	D�B	C�B	L�B	L�B	L�B	N�B	Q�B	T�B	VB	XB	ZB	[#B	[#B	]/B	_;B	`BB	aHB	bNB	bNB	cTB	bNB	cTB	aHB	ZB	YB	\)B	[#B	\)B	aHB	e`B	ffB	gmB	iyB	iyB	m�B	l�B	k�B	k�B	p�B	q�B	q�B	s�B	t�B	u�B	x�B	}�B	�B	�B	�+B	�7B	�JB	�DB	�JB	�PB	�JB	�JB	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�-B	�9B	�9B	�?B	�?B	�9B	�?B	�LB	�RB	�XB	�^B	�dB	�qB	�wB	�qB	�qB	�qB	�}B	��B	��B	ÖB	ÖB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�;B	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
	7B
DB
JB
JB
JB
PB
VB
VB
VB
\B
VB
VB
\B
\B
\B
\B
\B
bB
hB
hB
hB
hB
uB
uB
uB
uB
{B
�B
�B
�B
$B
(�B
6+222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  Bv�Bv�Bv�Bu�Bv�Bv�Bw�Bw�Bw�Bv�Bw�Bw�Bw�Bw�Bw�Bx�Bx�Bx�By�Bx�By�By�By�Bx�Bx�Bx�Bx�By�By�By�By�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�B{�B{�B{�Bz�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B~�B�B�=B��B�B�/B�B�B��B	7B{B�B�B.B;dBN�BbNBl�Bp�Bq�Br�Bq�Bq�Bt�Bp�Bk�BffBbNB_;B]/BYBR�BH�B?}B49B2-B0!B)�B�BB�`BƨB�9B��B��Bx�BW
BI�B8RBPB
�B
��B
jB
=qB
"�B
hB
  B	�B	��B	�B	�DB	n�B	aHB	`BB	\)B	P�B	@�B	/B	�B	{B	B��B�B�sB�TB�;B�)B�B��B��B��BǮBB��B�wB�^B�RB�LB�?B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�B�B��B��B��B��B��B�-B�FB�RB�dB�jBÖBǮB��B��B�
B�mB�B�yB�sB�mB�fB�mB�mB�B�B��B��B�B�yB�`B�TB�NB�NB�ZB�`B�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	B	B��B��B��B	B	DB	bB	{B	uB	hB	bB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	(�B	/B	9XB	=qB	>wB	?}B	@�B	A�B	D�B	D�B	D�B	C�B	L�B	L�B	L�B	N�B	Q�B	T�B	VB	XB	ZB	[#B	[#B	]/B	_;B	`BB	aHB	bNB	bNB	cTB	bNB	cTB	aHB	ZB	YB	\)B	[#B	\)B	aHB	e`B	ffB	gmB	iyB	iyB	m�B	l�B	k�B	k�B	p�B	q�B	q�B	s�B	t�B	u�B	x�B	}�B	�B	�B	�+B	�7B	�JB	�DB	�JB	�PB	�JB	�JB	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�-B	�9B	�9B	�?B	�?B	�9B	�?B	�LB	�RB	�XB	�^B	�dB	�qB	�wB	�qB	�qB	�qB	�}B	��B	��B	ÖB	ÖB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�;B	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
	7B
DB
JB
JB
JB
PB
VB
VB
VB
\B
VB
VB
\B
\B
\B
\B
\B
bB
hB
hB
hB
hB
uB
uB
uB
uB
{B
�B
�B
�B
$B
(�B
6+222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190539                              AO  ARCAADJP                                                                    20181005190539    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190539  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190539  QCF$                G�O�G�O�G�O�8000            
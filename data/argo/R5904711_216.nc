CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-16T19:08:11Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݌   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20230616190811  20230616190811  5904711 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  6217                            2B  A   NAVIS_A                         0487                            011514                          863 @ٚ��G�|1   @ٚۦ��@'�Q��c��7Kƨ1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B ffB��B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Duy�Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� DzfDz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@��\@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A��B �BQ�BQ�B�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��DuuDu��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dz�Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�z�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A��/A��HA��HA��HA��HA��TA��mA��`A��`A��mA��/A�+A�bNA׮A�-A��A�\)Aɏ\A�z�A�7LA�+A��-A�dZA�p�A�|�A�;dA�XA���A���A�oA���A��A�A��A���A�A���A���A�bA��uA�x�A��A�ĜA��A���A���A�C�A�1'A�A�A|�DAv1Ap�Amx�Ai�^Ah�Ae�A_`BAY��AT~�APv�AMdZAJbAE��AD�/AD�A@�yA>��A>�A=/A;C�A:�DA9x�A7|�A5�A5XA533A5A4��A4M�A4A3��A3l�A3XA3\)A2ĜA2E�A1��A0�A0{A/p�A.�A-�A,�A,�jA,�\A+�
A+�#A+��A+�A,�A,I�A+�-A+�A)��A(��A(M�A'��A&n�A$JA"�DA!&�A ffA�A?}A�yAn�AbA�-A33A�^AXA1A33A�Ar�AA?}A�A��AȴA�AoAO�A��AJA��A
=A��A�AA�Ap�A��AȴAz�A^5AVA{A  A  A�;A�^A��AhsA�A"�AoAZA�FA%A�/AhsA33A��A��A�;AS�A�`A��AVA��AdZA
�A
��A
�9A
ZA	�7A�/A��AbNAZAM�A��A+A��A�\A9XA|�AS�A�A��A��A�jA�A�AA�A�AA�A��A|�AC�A�`A��A�jAA�A��A ��A ��A ~�A n�A ZA E�A 9XA (�A   @�+@�hs@���@��@��@��@���@�"�@���@�-@��/@��m@�l�@��@�=q@��@���@�p�@�O�@���@�@���@�z�@�I�@� �@�@��@�^5@�h@� �@��H@��@�ƨ@�ȴ@�-@�^@�&�@�7L@�/@�j@�w@�l�@⟾@�{@��#@�x�@���@�Q�@�1'@���@ް!@ݺ^@�X@��/@ܣ�@�(�@��H@�^5@���@�@٩�@�V@�A�@�"�@�5?@Ցh@��@�bN@ӕ�@�l�@�C�@�@Ұ!@҇+@�E�@�@���@ёh@���@�1'@�  @��;@�K�@���@�~�@�$�@�{@���@Ͳ-@�X@��@���@̓u@�ƨ@�ȴ@�v�@�{@ɩ�@�x�@��@�bN@��
@ǍP@�+@�5?@��#@�@š�@�p�@��@ļj@ģ�@�Q�@�dZ@\@�-@��^@��u@�Z@�b@�ƨ@�S�@�5?@��7@�%@���@��9@��@�j@��F@��@��T@�&�@��u@�  @���@�K�@�
=@��y@��+@�ff@�V@��T@��@�V@��@�bN@��
@�@�V@��#@��#@���@��h@��@��@��@��;@�o@���@��+@�^5@��@�hs@���@��@�K�@��\@�V@��-@��@���@�1'@���@���@�|�@�+@��\@�M�@�$�@��@�O�@��@��9@�9X@�b@���@�"�@��!@�=q@���@���@��h@�7L@��`@��@�r�@�9X@�b@��F@���@�l�@�;d@���@��+@�n�@�^5@�@��T@���@�X@�V@��j@�r�@�A�@���@�
=@���@�V@��T@�x�@�/@��`@�Ĝ@��j@�(�@���@���@���@�33@��y@�v�@�E�@�J@��@�G�@��@�z�@�bN@��@���@�;d@�"�@��H@��!@���@�M�@��#@��@�hs@�G�@��@�Ĝ@��@�j@� �@�ƨ@���@�\)@���@��@��h@�O�@�&�@�%@�Ĝ@���@�r�@�Z@�(�@��P@�\)@�33@��y@��+@�E�@��@��-@��7@�7L@�%@��@�A�@��;@��P@�;d@��@���@�5?@��T@��^@���@��7@��@��u@�Q�@�9X@�b@��@��w@��P@�t�@�K�@�+@���@�n�@��@���@���@�hs@�G�@�?}@�7L@��@��@���@�Z@�A�@�9X@��@~�y@~E�@~V@~V@~E�@}�h@|��@|�/@|�j@|z�@|(�@{��@{�
@{�F@{o@zM�@z�@y�@y��@y�7@y�@x��@w�@wK�@w
=@v��@vff@vE�@v$�@v@u?}@tI�@s��@s�
@s��@s@q�7@p�u@p�u@p��@p��@pr�@p �@o�w@o\)@o�@nff@n$�@n{@m�@mp�@l�D@ko@j��@j~�@j�@i��@h�`@hbN@h  @g�w@gK�@f��@e@d��@d9X@d(�@d1@cƨ@cdZ@b��@bJ@a��@`��@`bN@`1'@`  @_��@_�@_;d@^�R@^��@^ff@^5?@^$�@]��@]/@]�@\�/@\�D@\(�@\�@[��@[�
@[�F@[��@[t�@[dZ@Z�H@Yhs@Xb@W��@W;d@V�R@VV@V{@U�@U�@T�@S��@St�@S"�@R~�@Q�^@QX@Q7L@P��@PbN@O�@O�@Ol�@OK�@O�@O
=@N�y@N�y@N�@N�R@N�+@Nv�@N$�@M�-@M��@M�@L�j@Lj@L(�@K�F@Kt�@KC�@Ko@Jn�@I��@I�@H��@H��@H�@HbN@HQ�@H  @G|�@GK�@G�@F�@F��@FE�@E�@E�-@E`B@D�@D��@D��@C��@C"�@B��@B��@B�\@BJ@A�^@Ax�@A&�@@�`@@��@@bN@@b@?��@?�@>��@>�y@>�R@>5?@=�@=��@=��@=�-@=��@=��@=?}@=V@<�/@<�D@<Z@<�@;�F@;C�@:�@:��@:�!@:-@9x�@8�@7�@7��@7+@6��@6�R@6v�@6E�@5�T@5O�@5V@4�/@4��@41@3�@3dZ@3"�@3@2�!@2M�@1��@1hs@1&�@1&�@1&�@1�@0�@0Q�@0A�@01'@0b@0  @/�w@/\)@/\)@/;d@/+@/�@.��@.�y@.�@.ȴ@.�R@.��@.v�@.V@.$�@.{@-�T@-�h@-�@-O�@-/@,�@,�j@,j@,I�@+��@+ƨ@+��@+t�@*��@*M�@*M�@*�@)��@)��@)�7@)G�@(�9@(Q�@(A�@( �@( �@( �@( �@(b@(  @'�;@'�@'+@&�@&v�@&{@%�-@%�@$�/@$�j@$�D@$j@$9X@$1@#�
@#�@#dZ@#S�@#C�@#o@"��@"n�@"-@!��@!�#@!�^@!hs@!G�@!G�@!7L@!%@ Ĝ@ Ĝ@ �9@ ��@ �u@ 1'@�;@�P@+@��@�y@ȴ@v�@V@5?@5?@5?@@�@@�@�@��@�@�/@�/@�@�D@Z@Z@(�@�
@��@��@��@��@dZ@"�@�@�@�!@^5@=q@�@J@�#@x�@G�@%@�`@��@Ĝ@�u@A�@b@��@��@l�@l�@K�@+@�y@�@��@�+@v�@5?@��@�-@�@�@��@�@�@z�@�m@�m@�
@��@t�@C�@"�@�@�H@��@��@��@~�@-@�@��@��@�7@hs@7L@%@��@�9@�9@��@�@bN@1'@�P@l�@\)@�@�R@$�@�T@@�-@�@p�@O�@�@�@�/@�j@�D@z�@j@I�@9X@(�@1@��@�
@��@S�@@
�@
�H@
�\@
~�@
~�@
M�@
=q@
J@	�@	��@	��@	X@	G�@	G�@	7L@	�@��@��@��@�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��A��/A��HA��HA��HA��HA��TA��mA��`A��`A��mA��/A�+A�bNA׮A�-A��A�\)Aɏ\A�z�A�7LA�+A��-A�dZA�p�A�|�A�;dA�XA���A���A�oA���A��A�A��A���A�A���A���A�bA��uA�x�A��A�ĜA��A���A���A�C�A�1'A�A�A|�DAv1Ap�Amx�Ai�^Ah�Ae�A_`BAY��AT~�APv�AMdZAJbAE��AD�/AD�A@�yA>��A>�A=/A;C�A:�DA9x�A7|�A5�A5XA533A5A4��A4M�A4A3��A3l�A3XA3\)A2ĜA2E�A1��A0�A0{A/p�A.�A-�A,�A,�jA,�\A+�
A+�#A+��A+�A,�A,I�A+�-A+�A)��A(��A(M�A'��A&n�A$JA"�DA!&�A ffA�A?}A�yAn�AbA�-A33A�^AXA1A33A�Ar�AA?}A�A��AȴA�AoAO�A��AJA��A
=A��A�AA�Ap�A��AȴAz�A^5AVA{A  A  A�;A�^A��AhsA�A"�AoAZA�FA%A�/AhsA33A��A��A�;AS�A�`A��AVA��AdZA
�A
��A
�9A
ZA	�7A�/A��AbNAZAM�A��A+A��A�\A9XA|�AS�A�A��A��A�jA�A�AA�A�AA�A��A|�AC�A�`A��A�jAA�A��A ��A ��A ~�A n�A ZA E�A 9XA (�A   @�+@�hs@���@��@��@��@���@�"�@���@�-@��/@��m@�l�@��@�=q@��@���@�p�@�O�@���@�@���@�z�@�I�@� �@�@��@�^5@�h@� �@��H@��@�ƨ@�ȴ@�-@�^@�&�@�7L@�/@�j@�w@�l�@⟾@�{@��#@�x�@���@�Q�@�1'@���@ް!@ݺ^@�X@��/@ܣ�@�(�@��H@�^5@���@�@٩�@�V@�A�@�"�@�5?@Ցh@��@�bN@ӕ�@�l�@�C�@�@Ұ!@҇+@�E�@�@���@ёh@���@�1'@�  @��;@�K�@���@�~�@�$�@�{@���@Ͳ-@�X@��@���@̓u@�ƨ@�ȴ@�v�@�{@ɩ�@�x�@��@�bN@��
@ǍP@�+@�5?@��#@�@š�@�p�@��@ļj@ģ�@�Q�@�dZ@\@�-@��^@��u@�Z@�b@�ƨ@�S�@�5?@��7@�%@���@��9@��@�j@��F@��@��T@�&�@��u@�  @���@�K�@�
=@��y@��+@�ff@�V@��T@��@�V@��@�bN@��
@�@�V@��#@��#@���@��h@��@��@��@��;@�o@���@��+@�^5@��@�hs@���@��@�K�@��\@�V@��-@��@���@�1'@���@���@�|�@�+@��\@�M�@�$�@��@�O�@��@��9@�9X@�b@���@�"�@��!@�=q@���@���@��h@�7L@��`@��@�r�@�9X@�b@��F@���@�l�@�;d@���@��+@�n�@�^5@�@��T@���@�X@�V@��j@�r�@�A�@���@�
=@���@�V@��T@�x�@�/@��`@�Ĝ@��j@�(�@���@���@���@�33@��y@�v�@�E�@�J@��@�G�@��@�z�@�bN@��@���@�;d@�"�@��H@��!@���@�M�@��#@��@�hs@�G�@��@�Ĝ@��@�j@� �@�ƨ@���@�\)@���@��@��h@�O�@�&�@�%@�Ĝ@���@�r�@�Z@�(�@��P@�\)@�33@��y@��+@�E�@��@��-@��7@�7L@�%@��@�A�@��;@��P@�;d@��@���@�5?@��T@��^@���@��7@��@��u@�Q�@�9X@�b@��@��w@��P@�t�@�K�@�+@���@�n�@��@���@���@�hs@�G�@�?}@�7L@��@��@���@�Z@�A�@�9X@��@~�y@~E�@~V@~V@~E�@}�h@|��@|�/@|�j@|z�@|(�@{��@{�
@{�F@{o@zM�@z�@y�@y��@y�7@y�@x��@w�@wK�@w
=@v��@vff@vE�@v$�@v@u?}@tI�@s��@s�
@s��@s@q�7@p�u@p�u@p��@p��@pr�@p �@o�w@o\)@o�@nff@n$�@n{@m�@mp�@l�D@ko@j��@j~�@j�@i��@h�`@hbN@h  @g�w@gK�@f��@e@d��@d9X@d(�@d1@cƨ@cdZ@b��@bJ@a��@`��@`bN@`1'@`  @_��@_�@_;d@^�R@^��@^ff@^5?@^$�@]��@]/@]�@\�/@\�D@\(�@\�@[��@[�
@[�F@[��@[t�@[dZ@Z�H@Yhs@Xb@W��@W;d@V�R@VV@V{@U�@U�@T�@S��@St�@S"�@R~�@Q�^@QX@Q7L@P��@PbN@O�@O�@Ol�@OK�@O�@O
=@N�y@N�y@N�@N�R@N�+@Nv�@N$�@M�-@M��@M�@L�j@Lj@L(�@K�F@Kt�@KC�@Ko@Jn�@I��@I�@H��@H��@H�@HbN@HQ�@H  @G|�@GK�@G�@F�@F��@FE�@E�@E�-@E`B@D�@D��@D��@C��@C"�@B��@B��@B�\@BJ@A�^@Ax�@A&�@@�`@@��@@bN@@b@?��@?�@>��@>�y@>�R@>5?@=�@=��@=��@=�-@=��@=��@=?}@=V@<�/@<�D@<Z@<�@;�F@;C�@:�@:��@:�!@:-@9x�@8�@7�@7��@7+@6��@6�R@6v�@6E�@5�T@5O�@5V@4�/@4��@41@3�@3dZ@3"�@3@2�!@2M�@1��@1hs@1&�@1&�@1&�@1�@0�@0Q�@0A�@01'@0b@0  @/�w@/\)@/\)@/;d@/+@/�@.��@.�y@.�@.ȴ@.�R@.��@.v�@.V@.$�@.{@-�T@-�h@-�@-O�@-/@,�@,�j@,j@,I�@+��@+ƨ@+��@+t�@*��@*M�@*M�@*�@)��@)��@)�7@)G�@(�9@(Q�@(A�@( �@( �@( �@( �@(b@(  @'�;@'�@'+@&�@&v�@&{@%�-@%�@$�/@$�j@$�D@$j@$9X@$1@#�
@#�@#dZ@#S�@#C�@#o@"��@"n�@"-@!��@!�#@!�^@!hs@!G�@!G�@!7L@!%@ Ĝ@ Ĝ@ �9@ ��@ �u@ 1'@�;@�P@+@��@�y@ȴ@v�@V@5?@5?@5?@@�@@�@�@��@�@�/@�/@�@�D@Z@Z@(�@�
@��@��@��@��@dZ@"�@�@�@�!@^5@=q@�@J@�#@x�@G�@%@�`@��@Ĝ@�u@A�@b@��@��@l�@l�@K�@+@�y@�@��@�+@v�@5?@��@�-@�@�@��@�@�@z�@�m@�m@�
@��@t�@C�@"�@�@�H@��@��@��@~�@-@�@��@��@�7@hs@7L@%@��@�9@�9@��@�@bN@1'@�P@l�@\)@�@�R@$�@�T@@�-@�@p�@O�@�@�@�/@�j@�D@z�@j@I�@9X@(�@1@��@�
@��@S�@@
�@
�H@
�\@
~�@
~�@
M�@
=q@
J@	�@	��@	��@	X@	G�@	G�@	7L@	�@��@��@��@�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A�+A�-A�(�A�(�A�$�A�$�A�$�A�$�A�&�A�-A�-A�?}A��A�x�A݃A�A�B ��A��PA�ĜA�VA� �A��B�hB�B@�BA�BG�BffBdZB�B�B�7B��B�}BÖB�-B�Bu�B��B�oBv�Bv�B��B��B�hB�Bk�BW
B;dB!�B\B��B�B�B�mB�/B��B��B��B��B�/B�TB�BBBBbB�B�B!�B7LB<jBK�Bk�B�B�bB�oB�{B�{B��B��B�B�FBÖBƨB��B��B�
B�B�)B�NB�B��B��B��B��BBDB�B�B)�B1'B49B49B49B5?B33B8RB7LB49B0!B/B0!B1'B1'B/B0!B1'B/B/B-B)�B+B-B2-B6FB:^B@�BE�BF�BK�BT�B\)BcTBhsBm�Bq�Bs�Bs�Br�Bt�Bx�Bz�B{�B}�B~�B� B�B�B�%B�%B�%B�1B�PB�bB�oB�oB�hB�\B�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�3B�3B�9B�?B�LB�RB�XB�XB�^B�jB�qB�qB�qB�qB�wB��B��B��BBĜBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�
B�
B�B�#B�)B�/B�5B�;B�;B�;B�;B�5B�BB�ZB�`B�fB�fB�fB�mB�mB�mB�yB�yB�B�B�B�B�B��B��B��B  BBBBBBB+B1B1B1B
=BJBPBVB\B\BhBuB{B�B�B�B�B�B�B�B�B!�B#�B$�B$�B&�B'�B(�B)�B+B-B.B0!B2-B2-B2-B49B6FB7LB8RB9XB9XB:^B;dB=qB=qB=qB?}BB�BC�BD�BE�BE�BG�BH�BJ�BL�BN�BR�BS�BT�BVBW
BXBZBYBZB]/B`BBaHBbNBffBgmBhsBhsBiyBk�Bm�Bp�Bq�Br�Bs�Bs�Bt�Bu�Bw�By�B{�B}�B~�B� B�B�B�B�B�%B�+B�1B�=B�DB�DB�JB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�3B�?B�FB�LB�RB�XB�dB�jB�wB�}B��B��BBĜBŢBǮBȴBɺB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�#B�)B�/B�/B�;B�;B�BB�HB�NB�TB�ZB�`B�fB�sB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��BBBBBB%B+B1B	7BDBDBJBPBVBbBoBoBoB{B�B�B�B�B�B�B�B�B �B �B!�B"�B#�B#�B$�B$�B%�B'�B(�B)�B+B-B-B/B0!B1'B2-B33B49B6FB7LB8RB:^B;dB<jB>wB?}B@�BA�BA�BB�BD�BE�BF�BF�BG�BH�BI�BJ�BJ�BK�BK�BN�BO�BQ�BQ�BR�BS�BT�BT�BT�BVBW
BXBYBYB[#B]/B^5B_;B_;B_;B`BBbNBbNBcTBdZBdZBe`Be`BffBgmBhsBiyBiyBjBk�Bl�Bm�Bn�Bp�Bq�Br�Bs�Bs�Bt�Bs�Bu�Bw�Bx�Bx�Bx�By�B{�B|�B}�B� B� B�B�B�B�B�B�%B�+B�+B�1B�7B�=B�PB�VB�VB�\B�\B�bB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�!B�'B�-B�-B�3B�3B�3B�9B�FB�XB�^B�dB�jB�qB�wB�}B��B��BBÖBĜBŢBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�#B�#B�)B�)B�5B�;B�HB�HB�HB�NB�TB�TB�ZB�fB�fB�mB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  BBBBBBBB%B%B+B	7B
=B
=B
=BDBJB\BbBhBoBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B"�B#�B#�B#�B%�B&�B&�B'�B'�B'�B(�B)�B+B+B,B-B-B.B.B/B/B0!B0!B1'B1'B2-B2-B33B49B49B5?B6FB6FB7LB7LB8RB9XB9XB:^B;dB=qB=qB=qB>wB?}B?}B@�BB�BC�BC�BD�BD�BD�BE�BE�BE�BF�BF�BH�BH�BI�BJ�BK�BM�BN�BN�BO�BP�BP�BQ�BR�BS�BS�BT�BT�BVBW
BXBXBYBZBZB[#B\)B\)B\)B]/B^5B^5B^5B_;B_;B`BBaHBbNBcTBcTBdZBdZBe`BffBgmBgmBgmBhsBhsBiyBiyBk�Bk�Bl�Bl�Bl�Bm�Bn�Bn�Bo�Bo�Bp�Bq�Bq�Bq�Br�Br�Bs�Bt�Bt�Bu�Bv�Bw�Bw�Bw�Bx�By�By�Bz�B{�B{�B{�B|�B}�B~�B� B� B�B�B�B�B�B�B�B�B�B�B�%B�+B�+B�1B�7B�7B�=B�=B�JB�JB�PB�PB�VB�\B�\B�bB�bB�hB�hB�oB�oB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�'B�-B�-B�-B�-B�3B�9B�9B�?B�FB�FB�F444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 A���A���A���A��A��A��A��A��A��A��A���A���A�
>A��HA�C�A�M�A�JB �RA�XA�]A��A��A�_Bv�B��B%�B&�B-BK�BI�BglBhrBn�B�1B��B��B��B�nB[#B�%Bw�B\)B\)B}�B|�Bv�BhrBP�B<jB �B+B��B�ZB�
B��B��BB�?B�3B�9B�XBBȴB��B�yB�B�lB��B��B��B+B�B!�B1'BP�BjBu�Bw�By�By�Bz�B�B�hB��B��B�B�3B�RB�jB�wB��BǮB��B�5B�GB�NB�ZB�yB�B��BB\B�B�B�B�B�B�B�B�B�B�B{B�B�B�B{B�B�B{B{BnB\BbBnB�B�B�B%�B+B,B1'B:^BA�BH�BM�BR�BW
BYBYBXBZB^5B`ABaGBcTBdZBe`BffBiyBk�Bk�Bk�Bm�Br�Bu�Bw�Bw�Bv�Bt�Bv�Bx�B� B�B�B�B�B�B�B�B�+B�%B�%B�%B�CB�\B�PB�IB�CB�IB�VB�\B�VB�nB�nB�nB�tB�tB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�'B�-B�-B�-B�-B�3B�LB�RB�XB�XB�^B�^B�jB�jB�jB�}B��B��BBÕBěBěBěBěBÕBŢBɺB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�#B�5B�NB�`B�fB�fB�rB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B  BBBB+B	7B
=B
=BIBPBVB\BbBnBtB�B�B�B�B�B�B�B�B�B�B�B �B"�B"�B"�B$�B'�B(�B)�B+B+B-B.B0!B2-B49B8RB9XB:^B;dB<jB=pB?}B>wB?}BB�BE�BF�BG�BK�BL�BM�BM�BN�BP�BR�BVBW
BXBYBYBZB[#B]/B_;BaGBcTBdZBe`BglBglBiyBjBk�Bl�Bm�Bo�Bp�Bp�Bq�Bu�Bw�Bz�B{�B}�B~�B�B�B�B�B�+B�1B�7B�7B�=B�CB�IB�VB�bB�tB�tB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�9B�9B�EB�LB�RB�XB�^B�dB�pB�pB�wB�wB��B��BBBěBěBŢBƨBǮBȴBɺB��B��B��B��B��B��B��B�B�B�B�B�#B�#B�)B�/B�;B�AB�GB�TB�ZB�fB�fB�yB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B  BB%B%B+B1B	7B	7B
=B
=BCBPBVB\BbBnBnB{B�B�B�B�B�B�B�B�B�B �B!�B#�B$�B%�B&�B&�B'�B)�B+B,B,B-B.B/B0!B0!B1'B1'B49B5?B7LB7LB8RB9XB:^B:^B:^B;dB<jB=pB>wB>wB@�BB�BC�BD�BD�BD�BE�BG�BG�BH�BI�BI�BJ�BJ�BK�BL�BM�BN�BN�BO�BP�BQ�BR�BS�BVBW
BXBYBYBZBYB[#B]/B^5B^5B^5B_;BaGBbNBcTBe`Be`BglBglBhrBiyBjBk�Bl�Bl�Bm�Bn�Bo�Br�Bs�Bs�Bt�Bt�Bu�Bv�Bw�Bx�Bz�B{�B|�B~�B�B�B�B�B�B�B�%B�+B�1B�=B�=B�CB�IB�IB�PB�VB�VB�\B�bB�bB�hB�nB�tB�tB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�-B�3B�9B�?B�?B�EB�LB�LB�RB�RB�RB�XB�^B�dB�jB�pB�wB�wB��B��B��B��BÕBěBƨBƨBƨBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�
B�B�B�B�#B�)B�)B�/B�/B�5B�AB�GB�NB�NB�NB�ZB�`B�`B�`B�fB�lB�lB�rB�rB�yB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBB%B1B1B	7B	7B	7BCBIBIBPBPBPBVB\BbBbBhBnBnBtBtB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B"�B"�B#�B$�B$�B%�B'�B(�B(�B)�B)�B)�B+B+B+B,B,B.B.B/B0!B1'B33B49B49B5?B6EB6EB7LB8RB9XB9XB:^B:^B;dB<jB=pB=pB>wB?}B?}B@�BA�BA�BA�BB�BC�BC�BC�BD�BD�BE�BF�BG�BH�BH�BI�BI�BJ�BK�BL�BL�BL�BM�BM�BN�BN�BP�BP�BQ�BQ�BQ�BR�BS�BS�BT�BT�BVBW
BW
BW
BXBXBYBZBZB[#B\)B]/B]/B]/B^5B_;B_;B`ABaGBaGBaGBbNBcTBdZBe`Be`BffBffBglBglBhrBhrBiyBiyBjBjBk�Bl�Bl�Bm�Bn�Bn�Bo�Bo�Bq�Bq�Br�Br�Bs�Bt�Bt�Bu�Bu�Bv�Bv�Bw�Bw�Bx�By�By�Bz�Bz�B{�B{�B|�B|�B}�B}�B~�B~�B~�B� B�B�B�B�B�B�B�B�%B�%B�+B�+B�1B�1B�7B�=B�=B�CB�IB�PB�PB�VB�VB�\B�\B�\B�bB�bB�hB�nB�nB�tB�tB�tB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0260000                                                                                                                                                                                                                                    Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                       PSAL ADJUST [dd mm yyyy N S_off stddev] 02 04 2020 160 -0.0260000 0.0000 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20230616190811              20230616190811  AO  ARCAADJP                                                                    20230616190811    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20230616190811    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230616190811  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230616190811  QCF$                G�O�G�O�G�O�8800            
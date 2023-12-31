CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-14T00:35:15Z creation;2018-09-14T00:35:20Z conversion to V3.1;2019-12-19T07:32:48Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180914003515  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_280                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؁5��| 1   @؁6�6� @9��l�C��dQ��#��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D¼�D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�C3D׀ D�� D�  D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�6f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��\@�@�A�HA=G�A^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A��A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB`�Bh�Bo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C��C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�Dº�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�@�D�}�D׽�D���D�:�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�@�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D� �D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�4)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�oA�oA�oA�
=A�1A�1A�
=A�
=A�JA�JA�bA�JA�1A�
=A�1A��A���A�ƨAִ9A֙�A�jA���A�-Aԉ7AҺ^A�C�A��HAɶFAư!A�O�A�ffA�x�A���A�I�A��hA�VA���A��FA�;dA���A���A��A��-A��-A���A��A��9A��A�A�A�ȴA���A��DA�p�A���A� �A��A��A�|�A��`A�ƨA�dZA�7LA�ĜA���A�  A�&�A��HA��A�/A�=qA�~�A�ƨA��A�VA�%A���A�
=A��A�7LA��A���A�VA�n�A�l�A��wA�$�A��RA�33A��A���A�
=A��!A��A�ffA�XA�^5A�Q�A�G�A�1'A���A�l�A���A��A��;A���A�A�A��A�33A��A��yA��wA��7A�bNA��A~^5A{�TAyS�Ax��Aw��Aw&�Av1'Au�PAt��At$�Ar��Aq�AqS�Apr�Ao�Am��Am/AkƨAi�Ait�Ai+Ah�!AgAf�Ad�Ac�Ab�A_/A]x�A[dZAX�AWS�AV(�AU�-AT�9ASK�AR�AQ�AQG�AP1'AL��AKl�AJ�!AJz�AJn�AJn�AJAH�jAG��AFȴAFQ�ADȴAC&�AB�A@(�A@1A?ƨA>E�A=�A<I�A;`BA;oA:�/A:�A:{A8�jA8 �A6�\A5\)A4=qA3��A2�A2��A2 �A1O�A0�jA0r�A0bNA/�A.��A-ƨA+�FA+&�A*�A*bA)dZA'33A%�wA#�A"ZA!\)A ZA�wA"�AM�Ap�A/A�A�jA��AQ�AC�AA��A��AĜAJA�wA�AĜAA"�AjA{A��At�A33A��A�A	��A9XA��A��AG�A"�AVA��A�RA��A��A�+A=qA��A%A�\A(�A  A��AdZAv�A��A�A �`A ff@��;@�ȴ@�hs@���@��j@�V@��
@�J@�@@���@�+@��^@��`@�9X@�F@�n�@��@�S�@�M�@���@�/@�r�@��@���@�I�@�t�@�@ݩ�@�ƨ@���@��T@��@�dZ@ա�@ӕ�@�ȴ@ҧ�@҇+@�M�@�1@Ώ\@���@���@�l�@�o@�{@�?}@ȓu@� �@�V@�"�@�@��@�b@���@�&�@���@�bN@��;@�\)@�"�@���@��@�X@���@��@�r�@�Z@���@�dZ@�"�@��H@�ff@�=q@��@���@�I�@��;@�"�@�$�@��/@�S�@�E�@�x�@��@�V@��@�r�@�I�@���@��F@���@�$�@��-@��@��9@�j@��@���@��-@��@���@�\)@���@�A�@�(�@�1'@�A�@�A�@�9X@�+@�p�@���@�Z@��@��@���@��\@�-@�@��#@��-@�G�@���@��@�"�@�ȴ@��T@�X@��`@�j@��@���@�{@���@���@�Q�@�I�@�I�@� �@���@��@�"�@���@�ff@���@�X@���@��9@�bN@�Q�@��@�S�@�33@�33@��@��H@�v�@��@�7L@��u@�b@��@�o@���@��@���@��7@�hs@�7L@���@��@��@�ƨ@�l�@��@���@��H@�~�@�5?@�{@�J@�@��@��@�/@��@��9@��@�z�@�z�@�A�@�;@|�@
=@~�R@~��@~�+@~E�@}@}O�@|�/@|�@|j@|(�@{ƨ@{t�@{C�@{o@z�H@z��@z~�@zn�@zM�@z�@y��@y�@y��@yx�@y�@x�9@xQ�@x1'@w�@w��@w\)@v�@vv�@u�T@u�h@u`B@u�@t�@t�/@t��@t9X@s�m@s�F@sdZ@r��@rM�@r�@q��@qhs@p��@pA�@p  @o�P@o\)@o+@o
=@n�@n��@nff@m@l�/@lj@l�@k�F@k��@k�@kdZ@j�H@j-@i�7@iX@i7L@i&�@i�@i%@h�u@hbN@hQ�@hA�@g�P@g\)@gK�@g;d@g�@f�@f�+@fE�@e�T@e��@ep�@e?}@d��@d�D@d9X@c�m@cƨ@c��@c�@cS�@c33@c@b��@b�\@bn�@b=q@b�@a��@ahs@`��@`Q�@`1'@`b@_�@_�;@_�;@_��@_�w@_��@_+@^ff@\�@\��@\��@\��@\�j@\�j@\��@\j@\9X@\(�@[�
@[��@[dZ@Z�@Z��@Z-@ZJ@Y�@Y��@Yx�@XĜ@X�u@X�u@X�u@W��@W;d@W�@W
=@V�R@V�+@Vff@V5?@U�@U�h@U`B@U/@T��@T�j@T�D@TZ@T(�@S�m@S�F@SS�@R�@R�@QX@Q&�@P��@P��@PA�@O|�@O�@N��@N��@Nff@Nff@N@M`B@MV@L�/@Lj@Kƨ@Kt�@K"�@J�H@JM�@I�^@Ix�@IG�@I%@H�@HQ�@HA�@G�;@G;d@F�+@E@E�@Ep�@E`B@E?}@D�j@DZ@D�@CdZ@B��@B~�@B^5@BM�@B�@A��@A�^@A��@A��@A��@A&�@@�9@@bN@?�;@?�P@?;d@?+@>ȴ@>��@>�+@=�h@<��@<��@<�@<�D@<Z@;�m@;ƨ@;ƨ@;��@;t�@;dZ@;S�@:�H@:~�@:�@:J@9��@9��@9��@9�7@9hs@8��@8�u@8�u@8bN@8A�@7��@7��@7|�@7\)@7;d@7
=@6�@6�+@6{@5��@5��@5?}@4�@4I�@4�@3��@3�@3S�@3@2��@2��@2�\@2~�@1�@1��@1�^@1X@1�@0��@0�9@0A�@0  @/�@/l�@/�@.�R@.��@.ff@.V@.5?@-@-?}@,��@,�@,�j@,��@,�@+�@+33@+@*�H@*�!@*~�@*=q@*J@)��@)��@)�@)�#@)��@)�^@)��@(��@(�@(bN@(bN@(Q�@(b@'��@'+@&�@&��@&��@&ff@&5?@%�@%@%��@%p�@%?}@%V@$�/@$�j@$��@$z�@$1@#�F@#�@#o@"��@"~�@"n�@"n�@"n�@"^5@"=q@"-@"-@"-@"-@"�@"�@"�@"J@!��@!hs@!&�@ ��@ Ĝ@ �u@ Q�@   @�;@�@;d@ȴ@��@ff@5?@��@`B@V@�@j@��@C�@��@J@��@�@�`@�9@��@��@Q�@1'@ �@�;@�;@��@�w@�@�P@\)@�@�+@V@5?@�@��@@�@V@��@1@ƨ@��@�H@�@�#@�^@�7@7L@��@��@��@��@Ĝ@�9@�9@��@�@ �@�@l�@+@�@��@��@�+@$�@@@�@�@?}@/@�@j@ƨ@�@dZ@C�@@
�H@
��@
��@
�!@
��@
�@	�@	�#@	��@	�^@	��@	X@	&�@	�@��@��@r�@A�@  @�;@�@�P@|�@l�@K�@�@�y@��@�+@ff@5?@�@�-@�@p�@`B@O�@?}@�@��@��@�j@�j@��@j@Z@I�@9X@(�@�@�@1@1@ƨ@t�@t�@dZ@S�@C�@33@��@�\@n�@^5@=q@�@J@�@��@�^@��@�7@hs@7L@�@%@ Ĝ@ �u@ r�@ Q�@ A�@ Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�oA�oA�oA�
=A�1A�1A�
=A�
=A�JA�JA�bA�JA�1A�
=A�1A��A���A�ƨAִ9A֙�A�jA���A�-Aԉ7AҺ^A�C�A��HAɶFAư!A�O�A�ffA�x�A���A�I�A��hA�VA���A��FA�;dA���A���A��A��-A��-A���A��A��9A��A�A�A�ȴA���A��DA�p�A���A� �A��A��A�|�A��`A�ƨA�dZA�7LA�ĜA���A�  A�&�A��HA��A�/A�=qA�~�A�ƨA��A�VA�%A���A�
=A��A�7LA��A���A�VA�n�A�l�A��wA�$�A��RA�33A��A���A�
=A��!A��A�ffA�XA�^5A�Q�A�G�A�1'A���A�l�A���A��A��;A���A�A�A��A�33A��A��yA��wA��7A�bNA��A~^5A{�TAyS�Ax��Aw��Aw&�Av1'Au�PAt��At$�Ar��Aq�AqS�Apr�Ao�Am��Am/AkƨAi�Ait�Ai+Ah�!AgAf�Ad�Ac�Ab�A_/A]x�A[dZAX�AWS�AV(�AU�-AT�9ASK�AR�AQ�AQG�AP1'AL��AKl�AJ�!AJz�AJn�AJn�AJAH�jAG��AFȴAFQ�ADȴAC&�AB�A@(�A@1A?ƨA>E�A=�A<I�A;`BA;oA:�/A:�A:{A8�jA8 �A6�\A5\)A4=qA3��A2�A2��A2 �A1O�A0�jA0r�A0bNA/�A.��A-ƨA+�FA+&�A*�A*bA)dZA'33A%�wA#�A"ZA!\)A ZA�wA"�AM�Ap�A/A�A�jA��AQ�AC�AA��A��AĜAJA�wA�AĜAA"�AjA{A��At�A33A��A�A	��A9XA��A��AG�A"�AVA��A�RA��A��A�+A=qA��A%A�\A(�A  A��AdZAv�A��A�A �`A ff@��;@�ȴ@�hs@���@��j@�V@��
@�J@�@@���@�+@��^@��`@�9X@�F@�n�@��@�S�@�M�@���@�/@�r�@��@���@�I�@�t�@�@ݩ�@�ƨ@���@��T@��@�dZ@ա�@ӕ�@�ȴ@ҧ�@҇+@�M�@�1@Ώ\@���@���@�l�@�o@�{@�?}@ȓu@� �@�V@�"�@�@��@�b@���@�&�@���@�bN@��;@�\)@�"�@���@��@�X@���@��@�r�@�Z@���@�dZ@�"�@��H@�ff@�=q@��@���@�I�@��;@�"�@�$�@��/@�S�@�E�@�x�@��@�V@��@�r�@�I�@���@��F@���@�$�@��-@��@��9@�j@��@���@��-@��@���@�\)@���@�A�@�(�@�1'@�A�@�A�@�9X@�+@�p�@���@�Z@��@��@���@��\@�-@�@��#@��-@�G�@���@��@�"�@�ȴ@��T@�X@��`@�j@��@���@�{@���@���@�Q�@�I�@�I�@� �@���@��@�"�@���@�ff@���@�X@���@��9@�bN@�Q�@��@�S�@�33@�33@��@��H@�v�@��@�7L@��u@�b@��@�o@���@��@���@��7@�hs@�7L@���@��@��@�ƨ@�l�@��@���@��H@�~�@�5?@�{@�J@�@��@��@�/@��@��9@��@�z�@�z�@�A�@�;@|�@
=@~�R@~��@~�+@~E�@}@}O�@|�/@|�@|j@|(�@{ƨ@{t�@{C�@{o@z�H@z��@z~�@zn�@zM�@z�@y��@y�@y��@yx�@y�@x�9@xQ�@x1'@w�@w��@w\)@v�@vv�@u�T@u�h@u`B@u�@t�@t�/@t��@t9X@s�m@s�F@sdZ@r��@rM�@r�@q��@qhs@p��@pA�@p  @o�P@o\)@o+@o
=@n�@n��@nff@m@l�/@lj@l�@k�F@k��@k�@kdZ@j�H@j-@i�7@iX@i7L@i&�@i�@i%@h�u@hbN@hQ�@hA�@g�P@g\)@gK�@g;d@g�@f�@f�+@fE�@e�T@e��@ep�@e?}@d��@d�D@d9X@c�m@cƨ@c��@c�@cS�@c33@c@b��@b�\@bn�@b=q@b�@a��@ahs@`��@`Q�@`1'@`b@_�@_�;@_�;@_��@_�w@_��@_+@^ff@\�@\��@\��@\��@\�j@\�j@\��@\j@\9X@\(�@[�
@[��@[dZ@Z�@Z��@Z-@ZJ@Y�@Y��@Yx�@XĜ@X�u@X�u@X�u@W��@W;d@W�@W
=@V�R@V�+@Vff@V5?@U�@U�h@U`B@U/@T��@T�j@T�D@TZ@T(�@S�m@S�F@SS�@R�@R�@QX@Q&�@P��@P��@PA�@O|�@O�@N��@N��@Nff@Nff@N@M`B@MV@L�/@Lj@Kƨ@Kt�@K"�@J�H@JM�@I�^@Ix�@IG�@I%@H�@HQ�@HA�@G�;@G;d@F�+@E@E�@Ep�@E`B@E?}@D�j@DZ@D�@CdZ@B��@B~�@B^5@BM�@B�@A��@A�^@A��@A��@A��@A&�@@�9@@bN@?�;@?�P@?;d@?+@>ȴ@>��@>�+@=�h@<��@<��@<�@<�D@<Z@;�m@;ƨ@;ƨ@;��@;t�@;dZ@;S�@:�H@:~�@:�@:J@9��@9��@9��@9�7@9hs@8��@8�u@8�u@8bN@8A�@7��@7��@7|�@7\)@7;d@7
=@6�@6�+@6{@5��@5��@5?}@4�@4I�@4�@3��@3�@3S�@3@2��@2��@2�\@2~�@1�@1��@1�^@1X@1�@0��@0�9@0A�@0  @/�@/l�@/�@.�R@.��@.ff@.V@.5?@-@-?}@,��@,�@,�j@,��@,�@+�@+33@+@*�H@*�!@*~�@*=q@*J@)��@)��@)�@)�#@)��@)�^@)��@(��@(�@(bN@(bN@(Q�@(b@'��@'+@&�@&��@&��@&ff@&5?@%�@%@%��@%p�@%?}@%V@$�/@$�j@$��@$z�@$1@#�F@#�@#o@"��@"~�@"n�@"n�@"n�@"^5@"=q@"-@"-@"-@"-@"�@"�@"�@"J@!��@!hs@!&�@ ��@ Ĝ@ �u@ Q�@   @�;@�@;d@ȴ@��@ff@5?@��@`B@V@�@j@��@C�@��@J@��@�@�`@�9@��@��@Q�@1'@ �@�;@�;@��@�w@�@�P@\)@�@�+@V@5?@�@��@@�@V@��@1@ƨ@��@�H@�@�#@�^@�7@7L@��@��@��@��@Ĝ@�9@�9@��@�@ �@�@l�@+@�@��@��@�+@$�@@@�@�@?}@/@�@j@ƨ@�@dZ@C�@@
�H@
��@
��@
�!@
��@
�@	�@	�#@	��@	�^@	��@	X@	&�@	�@��@��@r�@A�@  @�;@�@�P@|�@l�@K�@�@�y@��@�+@ff@5?@�@�-@�@p�@`B@O�@?}@�@��@��@�j@�j@��@j@Z@I�@9X@(�@�@�@1@1@ƨ@t�@t�@dZ@S�@C�@33@��@�\@n�@^5@=q@�@J@�@��@�^@��@�7@hs@7L@�@%@ Ĝ@ �u@ r�@ Q�@ A�@ Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�^B�RB�XB�RB�^B�dB�dB�^B�^B�^B�^B�XB�XB�XB�XB�XB��BǮBɺB��BȴBB��B��BB��B7LBo�Bx�B��B��B�Bw�B~�B�hB}�B�JB�JBe`BhsB�B�Bx�Bn�BaHBW
BP�BZBp�Bw�B�B�Bv�BZB`BB� Bz�Bs�BbNBH�BQ�B:^B�B�BBVBoBB�`B�mB��B�B�B�)B��B��BƨB��BɺBĜB�wB�B��B�Be`Bv�Bo�BXB'�B�B:^B:^B>wB?}B?}B?}B;dB5?B+B�B
��B
�}B
�B
�qB
�B
��B
z�B
k�B
_;B
}�B
z�B
r�B
k�B
^5B
<jB
%�B
\B
49B
,B
)�B
�B
�B
�B
oB
+B
	7B
uB
%B	��B	�B	��B	�B	�B	�`B	�ZB	�B	�}B	ŢB	�?B	��B	��B	v�B	e`B	XB	5?B	Q�B	H�B	M�B	C�B	1'B	33B	6FB	2-B	�B�B	bB	�B	)�B	)�B	&�B	�B	PB	hB	B		7B��B��B��B�B	B��B�`B�HB�B�fB�B�B�fB�#BȴBɺB�^B�FB�jBB��BǮBĜB�wB�qBB��B�3B��B��B�B��B��B�PB�BbNB_;BVBl�Bq�BjBr�Bq�Bl�BiyBs�Bo�BhsBXB@�BB�B6FB�BA�BB�BF�BO�BN�BA�B?}B<jB>wBI�BE�BG�BB�B9XB(�B
=B#�B=qBB�BA�BD�BF�BD�BA�BD�BC�B=qB7LB.B-B0!B33B5?B/B+B�B�B'�B)�B"�B�B�BPB	7B�B��BBJBuB�B�B �B�B�B�B�B\BVBbB�B�B�B�B�BJBhB�B�BbBDB�B�B�BbBbBuB �B)�B&�B �B\B{B�B �B(�B(�B"�B$�B"�B!�BVB�B(�B&�B�B%�B�B1'B49B2-B2-B49B1'B2-B33B7LB9XB=qB;dB7LB:^B<jB<jB:^B=qB:^B49B6FB:^B8RB2-B49B5?B@�BE�BN�BS�BP�BS�BS�BR�BQ�BM�BN�BT�BVBYBZBXBP�BN�BYBYBbNBYBaHBy�B{�B{�By�Bv�Bm�BhsB}�B�B�+B�=B�+B�VB�oB��B��B��B�uB�{B�bB�uB��B��B��B��B��B��B�B�B�RB�XBĜB��BɺBǮBǮBƨBŢB��B��B��B��B�B�B�B�5B�)B�B�`B�fB�`B�TB�TB�ZB�TB�B�B��B��B��B	B	%B	JB	JB	JB	JB	VB	PB	uB	�B	�B	�B	�B	�B	#�B	(�B	+B	+B	+B	(�B	.B	49B	49B	9XB	<jB	=qB	;dB	<jB	A�B	C�B	F�B	I�B	I�B	H�B	H�B	K�B	M�B	P�B	Q�B	S�B	VB	XB	[#B	\)B	]/B	^5B	`BB	aHB	bNB	cTB	cTB	dZB	dZB	dZB	e`B	gmB	iyB	l�B	l�B	n�B	m�B	o�B	q�B	q�B	t�B	v�B	w�B	y�B	{�B	|�B	}�B	�B	�B	�B	�B	�+B	�7B	�7B	�=B	�JB	�JB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�-B	�?B	�RB	�LB	�FB	�^B	�jB	�jB	�jB	�jB	�qB	�wB	�}B	��B	B	ÖB	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�NB	�TB	�TB	�NB	�NB	�HB	�NB	�HB	�TB	�NB	�TB	�ZB	�ZB	�`B	�`B	�mB	�sB	�sB	�mB	�mB	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B
  B	��B
B
B
B
B
B
B
B
B
B
	7B
	7B
	7B
	7B
DB
JB

=B
	7B

=B
JB
bB
hB
hB
bB
\B
bB
hB
\B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
 �B
!�B
 �B
�B
 �B
!�B
#�B
$�B
#�B
%�B
%�B
$�B
#�B
$�B
'�B
&�B
&�B
%�B
'�B
(�B
(�B
(�B
(�B
(�B
'�B
'�B
)�B
)�B
(�B
(�B
)�B
,B
,B
.B
.B
-B
.B
0!B
/B
/B
-B
0!B
0!B
/B
0!B
0!B
1'B
/B
1'B
1'B
2-B
2-B
2-B
49B
49B
5?B
49B
33B
33B
5?B
6FB
6FB
5?B
49B
5?B
7LB
9XB
9XB
9XB
9XB
:^B
:^B
<jB
<jB
<jB
;dB
;dB
;dB
:^B
8RB
:^B
<jB
=qB
<jB
<jB
;dB
:^B
=qB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
@�B
A�B
B�B
B�B
B�B
E�B
F�B
G�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
F�B
F�B
E�B
D�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
G�B
H�B
J�B
J�B
J�B
H�B
I�B
J�B
J�B
K�B
K�B
I�B
K�B
K�B
M�B
M�B
O�B
O�B
P�B
P�B
O�B
P�B
P�B
P�B
Q�B
R�B
Q�B
Q�B
Q�B
P�B
P�B
O�B
R�B
S�B
S�B
T�B
T�B
R�B
Q�B
Q�B
R�B
T�B
T�B
R�B
T�B
XB
YB
YB
ZB
ZB
\)B
\)B
\)B
[#B
[#B
\)B
\)B
[#B
ZB
[#B
ZB
\)B
]/B
]/B
\)B
]/B
]/B
^5B
_;B
^5B
]/B
^5B
_;B
^5B
]/B
^5B
`BB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
bNB
aHB
cTB
e`B
e`B
dZB
dZB
dZB
dZB
e`B
dZB
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
m�B
m�B
m�B
m�B
m�B
l�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
s�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�DB�RB�XB�lB�xB�dB�dB�^B�^B�^B�^B�XB�XB�XB�rB��B��B��B��B�)B�lB�3B�pB�FB�_B�lB<�Bv`B~wB�\B�DB��B}�B�3B�2B��B�vB��BkBm]B�gB�SB{0Bp�BcnBY1BS�B\Bq�Bx�B��B�oBw�B]dBa�B�iB{�BtnBdBJ�BS@B=<B=B!�B�BB[B3B�8B�_B��B�sBںBܬB��B��B�BªB�XB�mB�cB��B��B��Bi*BxBqBZ7B-B�B;dB;0B>�B?�B?�B?}B;B5�B+kB�B
�jB
�B
�vB
��B
�oB
��B
~wB
o5B
a|B
~]B
{JB
sMB
l=B
_�B
>�B
(�B
oB
5B
-B
*�B
B
 �B
�B
�B
	B

XB
B
zB	��B	�vB	��B	�cB	�7B	��B	��B	�	B	�oB	ƎB	�LB	�8B	��B	z^B	g�B	Z�B	8�B	SuB	JXB	N�B	D�B	2�B	4�B	6�B	2�B	�B�B	�B	�B	*0B	*0B	'8B	�B	(B	�B	�B	
#B�B��B��B��B	 B��B�RB��B�B�B�B��B�B��BʌB��B��B��B��B�{B�[B�KB�SB�}B�BB��B��B�B�HB�EB��B�OB�WB�pB��Be`Ba�BYBm�Br�Bk�Bs�Br�Bm�Bj�BtBp;Bi*BYBB�BD3B8�BQBBABDBG�BP}BO\BB�B@�B=�B?}BJ=BFYBH1BCB:*B*�B�B%`B=�BB�BB'BD�BF�BD�BBBD�BC�B=�B7�B/5B-�B0�B3�B5�B/�B+�B�B�B(�B*B#�BjBQBpB
�B��B��B�BjBaBCBVB!-BCB=B#B	BbB\B�BSB!B=B+B$B�BoB1BB�B�BEBKB_B�B�B�B!bB*0B'8B!|B B�B�B!|B)_B)yB#�B%zB#�B"�B}B�B)_B'�B	B&�B B1�B4�B2�B2�B4�B1�B2�B3�B7�B9�B=�B;�B7�B:�B<�B<�B:�B=�B:�B4�B6�B:�B9	B33B5ZB6�BA;BFYBOBBT,BQNBTFBT,BS[BRTBN�BOvBUgBV�BYBZ�BX�BQ�BO�BY�BY�BcBZQBbNBy�B|B|By�BwBn�Bi�B~BB��B��B��B��B��B��B��B��B��B��B��B�4B�,B�B�]B�bB�XB�yB��B��B��B��B�B��B��B��B��B��B�B�B�B�)B�dB�TB�mB�KBچB�OB�xBںB�`B�B�B�B�B��B�&B�B�B�FB�PB�cB	�B	tB	~B	~B	~B	�B	�B	�B	�B	�B	B	�B	�B	B	$B	)B	+6B	+B	+6B	)_B	.cB	4TB	4�B	9rB	<�B	=�B	;�B	<�B	A�B	C�B	F�B	I�B	I�B	IB	IB	LB	N"B	Q B	R:B	T,B	V9B	X_B	[WB	\CB	]IB	^jB	`\B	abB	b�B	cnB	cnB	d�B	dtB	d�B	e�B	g�B	i�B	l�B	l�B	n�B	m�B	o�B	q�B	q�B	t�B	v�B	xB	y�B	|B	}B	~(B	� B	�'B	�AB	�UB	�_B	�RB	�lB	�rB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�&B	�B	��B	��B	�,B	�2B	�*B	�IB	�5B	�[B	�GB	�aB	�aB	�tB	�lB	�fB	��B	�xB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	ðB	��B	żB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	� B	�B	�4B	�4B	�B	�+B	�+B	�KB	�7B	�7B	�B	�1B	�EB	�SB	�mB	׍B	�NB	�nB	�TB	�hB	�NB	�bB	�hB	�bB	�nB	�B	�nB	�B	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�B	�*B	�B
 B	�.B
 B
 B
 4B	�BB
'B
AB
[B
UB
3B
9B
SB
mB
SB
	RB
	RB
	RB
	�B
^B
~B

�B
	�B

�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
 �B
!�B
 �B
 �B
!�B
 �B
�B
!B
!�B
#�B
$�B
#�B
%�B
%�B
$�B
$&B
%B
(
B
'B
'B
&B
(
B
)B
)B
)B
)*B
)B
(>B
($B
*0B
*B
)*B
)DB
*KB
,"B
,=B
.IB
./B
-)B
./B
0;B
/5B
/5B
-CB
0!B
0;B
/OB
0UB
0UB
1AB
/iB
1AB
1[B
2GB
2aB
2aB
4nB
4TB
5ZB
4TB
3�B
3�B
5tB
6`B
6zB
5ZB
4nB
5�B
7�B
9�B
9�B
9�B
9�B
:xB
:xB
<�B
<jB
<�B
;dB
;B
;B
:xB
8�B
:xB
<�B
=�B
<�B
<�B
;B
:�B
=�B
>�B
?�B
?�B
?�B
?�B
@�B
A�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
@�B
A�B
B�B
B�B
B�B
E�B
F�B
G�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
F�B
F�B
E�B
D�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
G�B
H�B
J�B
J�B
J�B
IB
J	B
J�B
KB
K�B
K�B
J#B
K�B
LB
NB
N"B
O�B
O�B
Q B
P�B
O�B
QB
QB
Q B
RB
R�B
Q�B
RB
RB
QB
QB
PB
SB
TB
TB
U2B
UB
S&B
R B
R:B
S&B
UB
UB
S@B
U2B
X+B
YKB
Y1B
Z7B
Z7B
\)B
\)B
\)B
[=B
[#B
\CB
\)B
[=B
ZkB
[WB
ZkB
\CB
]/B
]IB
\CB
]dB
]IB
^OB
_;B
^OB
]~B
^OB
_VB
^jB
]dB
^�B
`\B
bhB
bhB
bhB
cnB
cTB
cnB
c�B
bhB
a�B
cnB
ezB
e`B
dtB
d�B
d�B
d�B
ezB
dtB
e�B
e�B
e�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
hsB
h�B
h�B
h�B
h�B
i�B
jB
jB
jB
jB
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
m�B
m�B
m�B
m�B
m�B
l�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
s�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809180035132018091800351320180918003513201809180200162018091802001620180918020016201809190030392018091900303920180919003039  JA  ARFMdecpA19c                                                                20180914093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180914003515  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180914003518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180914003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180914003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180914003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180914003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180914003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180914003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180914003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20180914005539                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180914153444  CV  JULD            G�O�G�O�F�	�                JM  ARCAJMQC2.0                                                                 20180917153513  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180917153513  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180917170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180918153039  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                
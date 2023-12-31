CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-09-28T17:02:07Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        =���   axis      Z        \  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   E�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  H�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   U@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  XX   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  d�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �<   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �l   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    Ę   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    Ĝ   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    Ġ   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    Ĥ   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  Ĩ   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20170928170207  20170928170207  5904057 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5006                            2B  A   NAVIS_A                         0305                            082713                          863 @�)���1   @�)�<��y@7?|�hs�d6�Q�1   GPS     Primary sampling: mixed [deep: discrete, shallow: continuous]                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ Dă3D�� D�  D�@ Dŀ D�� D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�B �B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB`�Bg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�zC�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�zC1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��=C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#uD#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:uD:��D;{�D;��D<{�D=�D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DY�DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dw�Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�DĀ�DĽ�D���D�=�D�}�DŽ�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�n�A�n�A�n�A�r�A�x�A�~�A߅AߋDAߣ�A�K�AދDA��TA�
=A� �A��Aۧ�A���Aڕ�Aڲ-A�ȴA�~�A�v�A�A�ȴA֡�AվwA�JAӮA��Aѕ�A�M�AЬA�(�AΑhAɶFA��A�hsA� �A�p�A�C�A��A�Aô9A�\)A�
=A��mA��PA�`BA�ȴA��/A���A�oA�|�A��A���A�1'A���A��RA�$�A��A��9A�z�A�;dA��DA�K�A��-A���A�M�A���A��A��A�=qA���A�I�A��A��FA�oA���A�7LA�Q�A�E�A���A��A�^5A�1'A�S�A�\)A�7LA��jA�^5A�&�A�E�A�`BA�1'A��RA�`BA���A��/A��A�r�A�=qA��A�1A��yA��A��A��uA��TA���A�(�A���A�XA�-A�hsA��yA�+A}��A}hsA|�+A{�7Az�+Ax��Av��Au��Aq�TApbNApAoC�An�Al$�Aj��Aj9XAh5?Ad9XAc`BA^Q�AZ�DAX=qAV��AVjAVZAVI�AV �AU"�AT�jAT��ATI�AShsASVAR��AR�yAR��ARr�AQ�mAO�7AK?}AJ�!AI
=AG�-AFz�AEoAD�AC��AB�9AA;dA@�/A?ƨA=��A:n�A9t�A6�A4 �A2��A0�+A/l�A/oA.�\A.9XA-�A,VA,9XA+�A+O�A+�A*I�A(^5A&r�A&Q�A%�;A$�\A#�FA!VAbAVAK�A33A�;A�A��A�A�`A  Av�A�
A%AE�A�TAA�^A��AC�AjA�mA7LA
�\A	VA�AdZA�+A5?A�-Al�A��AA�-A;dA��A~�AE�A��A�#A��AK�A%A �RA �+@�dZ@��R@��@�x�@�7L@�&�@�r�@�-@�  @�{@�p�@�@���@�A�@���@��@�dZ@��y@�^5@�x�@�Z@�!@�x�@�hs@�G�@���@�+@�&�@ᙚ@�o@�=q@�&�@���@ڗ�@���@�{@�J@�J@ّh@�Q�@�b@׮@ו�@���@��#@�X@Դ9@�|�@ҏ\@�~�@�^5@�5?@�@щ7@�%@�r�@��;@�\)@�~�@˝�@��@�`B@ț�@�1'@�1'@ǍP@�33@���@�{@��@š�@ċD@�M�@�ff@�G�@�v�@��-@�7L@��j@��!@��T@�7L@��/@��9@��9@��j@�Ĝ@��@��@��`@���@�z�@��@���@���@��w@�ƨ@���@�^5@���@�\)@��@�9X@�1'@�r�@��D@��@���@�S�@�V@�M�@�V@�@��@���@�K�@��H@��R@���@�$�@���@���@��@��@���@�@�J@�J@�{@��@�$�@��@��@���@�&�@���@�z�@��;@�dZ@��@���@�E�@�@���@��7@��@�%@���@���@���@���@��@��@���@��@�r�@�Z@�I�@�b@�|�@�C�@�;d@�"�@���@�-@���@�hs@�O�@�&�@���@�r�@��@��P@�C�@��@��@�Z@��@��\@��\@�33@���@���@��9@�Ĝ@�z�@� �@�ƨ@�+@�+@�@�n�@�5?@��T@�O�@�?}@�&�@���@���@���@�r�@�t�@���@�E�@�$�@��@�X@�&�@���@�t�@�K�@�C�@�C�@�C�@�;d@��@���@���@�n�@�J@���@���@�@�@�J@���@��@���@�A�@�b@��w@�\)@��@��y@��H@��H@�ȴ@���@��!@��+@�E�@�$�@��@��^@�x�@�?}@�%@��@���@���@�t�@�;d@��@��R@���@�ff@�5?@���@���@�G�@��`@��9@��D@�Q�@��@�33@��\@�n�@��@��@��#@��h@�X@�7L@�%@���@�1'@\)@~v�@~@}@}�-@}�-@}��@}��@}�@}?}@}V@|��@|j@{�m@{ƨ@{S�@z~�@z-@yX@xĜ@x��@x �@w�w@w�P@w
=@v$�@u�h@t��@t�@sdZ@s"�@so@so@s@r�@r��@r�\@rn�@rM�@r�@q��@qx�@q&�@o��@o+@n��@n$�@m@m�h@m`B@m�@l��@l�/@l��@lZ@k�m@kƨ@kt�@j�H@j��@j~�@j-@iX@hQ�@h  @gK�@g
=@f��@fff@f$�@f{@f$�@f{@e��@e��@e�@e�@e@e�-@ep�@e`B@eO�@d�@d9X@d1@d�@d1@c��@c��@c��@c��@c�m@c�F@c�@ct�@ct�@ct�@cS�@bn�@a��@aG�@a%@`��@`��@`Ĝ@`A�@_�w@_|�@^�@^�+@^v�@^E�@]@]`B@]V@\j@\j@\I�@\�@\1@[�
@[�
@[��@[dZ@Z^5@Y7L@X  @W|�@Wl�@W\)@WK�@WK�@WK�@WK�@W;d@W+@W
=@V��@Vv�@U@T�/@Tz�@Tz�@TZ@T9X@S�m@Sƨ@S��@S33@R�@Rn�@QX@P�9@PA�@O��@O+@O�@N�R@N�R@N�R@N��@N��@N��@N��@N�+@Nff@M�-@Mp�@Mp�@M`B@M`B@MV@L��@L�/@L�j@L�D@Lj@LZ@L9X@L9X@L(�@L�@KdZ@K"�@J�!@I��@I�7@I7L@HA�@G�@G�w@G��@Gl�@Fff@E�T@E�-@E`B@EV@D�@Dz�@C�
@Cƨ@C��@B�!@A��@AG�@A�@@�9@@Q�@@1'@@  @?�@?�w@?l�@?;d@>ȴ@>v�@<�j@<I�@;�F@:M�@:J@9��@:J@9��@9�@9��@9��@9��@9�7@9�7@9hs@9X@9G�@9G�@9G�@97L@9�@8��@8��@9&�@9%@9x�@9hs@9hs@9X@9�@9�@8��@8�`@8�`@8��@8��@8r�@81'@81'@8A�@8  @7�;@7�@7;d@7;d@7;d@7�@7
=@6��@6ȴ@5�h@5`B@5`B@5O�@5/@4�/@4(�@3dZ@1G�@0 �@.V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�n�A�n�A�n�A�r�A�x�A�~�A߅AߋDAߣ�A�K�AދDA��TA�
=A� �A��Aۧ�A���Aڕ�Aڲ-A�ȴA�~�A�v�A�A�ȴA֡�AվwA�JAӮA��Aѕ�A�M�AЬA�(�AΑhAɶFA��A�hsA� �A�p�A�C�A��A�Aô9A�\)A�
=A��mA��PA�`BA�ȴA��/A���A�oA�|�A��A���A�1'A���A��RA�$�A��A��9A�z�A�;dA��DA�K�A��-A���A�M�A���A��A��A�=qA���A�I�A��A��FA�oA���A�7LA�Q�A�E�A���A��A�^5A�1'A�S�A�\)A�7LA��jA�^5A�&�A�E�A�`BA�1'A��RA�`BA���A��/A��A�r�A�=qA��A�1A��yA��A��A��uA��TA���A�(�A���A�XA�-A�hsA��yA�+A}��A}hsA|�+A{�7Az�+Ax��Av��Au��Aq�TApbNApAoC�An�Al$�Aj��Aj9XAh5?Ad9XAc`BA^Q�AZ�DAX=qAV��AVjAVZAVI�AV �AU"�AT�jAT��ATI�AShsASVAR��AR�yAR��ARr�AQ�mAO�7AK?}AJ�!AI
=AG�-AFz�AEoAD�AC��AB�9AA;dA@�/A?ƨA=��A:n�A9t�A6�A4 �A2��A0�+A/l�A/oA.�\A.9XA-�A,VA,9XA+�A+O�A+�A*I�A(^5A&r�A&Q�A%�;A$�\A#�FA!VAbAVAK�A33A�;A�A��A�A�`A  Av�A�
A%AE�A�TAA�^A��AC�AjA�mA7LA
�\A	VA�AdZA�+A5?A�-Al�A��AA�-A;dA��A~�AE�A��A�#A��AK�A%A �RA �+@�dZ@��R@��@�x�@�7L@�&�@�r�@�-@�  @�{@�p�@�@���@�A�@���@��@�dZ@��y@�^5@�x�@�Z@�!@�x�@�hs@�G�@���@�+@�&�@ᙚ@�o@�=q@�&�@���@ڗ�@���@�{@�J@�J@ّh@�Q�@�b@׮@ו�@���@��#@�X@Դ9@�|�@ҏ\@�~�@�^5@�5?@�@щ7@�%@�r�@��;@�\)@�~�@˝�@��@�`B@ț�@�1'@�1'@ǍP@�33@���@�{@��@š�@ċD@�M�@�ff@�G�@�v�@��-@�7L@��j@��!@��T@�7L@��/@��9@��9@��j@�Ĝ@��@��@��`@���@�z�@��@���@���@��w@�ƨ@���@�^5@���@�\)@��@�9X@�1'@�r�@��D@��@���@�S�@�V@�M�@�V@�@��@���@�K�@��H@��R@���@�$�@���@���@��@��@���@�@�J@�J@�{@��@�$�@��@��@���@�&�@���@�z�@��;@�dZ@��@���@�E�@�@���@��7@��@�%@���@���@���@���@��@��@���@��@�r�@�Z@�I�@�b@�|�@�C�@�;d@�"�@���@�-@���@�hs@�O�@�&�@���@�r�@��@��P@�C�@��@��@�Z@��@��\@��\@�33@���@���@��9@�Ĝ@�z�@� �@�ƨ@�+@�+@�@�n�@�5?@��T@�O�@�?}@�&�@���@���@���@�r�@�t�@���@�E�@�$�@��@�X@�&�@���@�t�@�K�@�C�@�C�@�C�@�;d@��@���@���@�n�@�J@���@���@�@�@�J@���@��@���@�A�@�b@��w@�\)@��@��y@��H@��H@�ȴ@���@��!@��+@�E�@�$�@��@��^@�x�@�?}@�%@��@���@���@�t�@�;d@��@��R@���@�ff@�5?@���@���@�G�@��`@��9@��D@�Q�@��@�33@��\@�n�@��@��@��#@��h@�X@�7L@�%@���@�1'@\)@~v�@~@}@}�-@}�-@}��@}��@}�@}?}@}V@|��@|j@{�m@{ƨ@{S�@z~�@z-@yX@xĜ@x��@x �@w�w@w�P@w
=@v$�@u�h@t��@t�@sdZ@s"�@so@so@s@r�@r��@r�\@rn�@rM�@r�@q��@qx�@q&�@o��@o+@n��@n$�@m@m�h@m`B@m�@l��@l�/@l��@lZ@k�m@kƨ@kt�@j�H@j��@j~�@j-@iX@hQ�@h  @gK�@g
=@f��@fff@f$�@f{@f$�@f{@e��@e��@e�@e�@e@e�-@ep�@e`B@eO�@d�@d9X@d1@d�@d1@c��@c��@c��@c��@c�m@c�F@c�@ct�@ct�@ct�@cS�@bn�@a��@aG�@a%@`��@`��@`Ĝ@`A�@_�w@_|�@^�@^�+@^v�@^E�@]@]`B@]V@\j@\j@\I�@\�@\1@[�
@[�
@[��@[dZ@Z^5@Y7L@X  @W|�@Wl�@W\)@WK�@WK�@WK�@WK�@W;d@W+@W
=@V��@Vv�@U@T�/@Tz�@Tz�@TZ@T9X@S�m@Sƨ@S��@S33@R�@Rn�@QX@P�9@PA�@O��@O+@O�@N�R@N�R@N�R@N��@N��@N��@N��@N�+@Nff@M�-@Mp�@Mp�@M`B@M`B@MV@L��@L�/@L�j@L�D@Lj@LZ@L9X@L9X@L(�@L�@KdZ@K"�@J�!@I��@I�7@I7L@HA�@G�@G�w@G��@Gl�@Fff@E�T@E�-@E`B@EV@D�@Dz�@C�
@Cƨ@C��@B�!@A��@AG�@A�@@�9@@Q�@@1'@@  @?�@?�w@?l�@?;d@>ȴ@>v�@<�j@<I�@;�F@:M�@:J@9��@:J@9��@9�@9��@9��@9��@9�7@9�7@9hs@9X@9G�@9G�@9G�@97L@9�@8��@8��@9&�@9%@9x�@9hs@9hs@9X@9�@9�@8��@8�`@8�`@8��@8��@8r�@81'@81'@8A�@8  @7�;@7�@7;d@7;d@7;d@7�@7
=@6��@6ȴ@5�h@5`B@5`B@5O�@5/@4�/@4(�@3dZ@1G�@0 �@.V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B  B�BZB~�B}�BjBR�BJ�BB�B>wB;dBO�Br�Bk�BG�B=qB9XBK�Bz�BffB��B�'B�!B�B�B��B��B��B�3B�-B�FB�RB�RB�RB�RB�XB�XB�RB�^BĜBŢBĜBBB��B��B�jB�dB�wB�jB�qB�}B�wB�jB�XB��B��B��B�bB�B� Bz�By�Bt�BhsB`BB^5B^5B\)BXBL�BE�B=qB2-B.B$�BuB1B��B�B�B��B��B��B�LB�B��B��B�bB�%Bz�Bp�BiyBdZBI�B$�B
��B
�yB
�`B
��B
��B
��B
�RB
�B
��B
��B
�B
{�B
p�B
cTB
^5B
YB
P�B
I�B
>wB
0!B
&�B
hB
%B
B	��B	�B	�TB	�B	��B	ĜB	��B	��B	�B	k�B	^5B	XB	W
B	W
B	VB	VB	R�B	P�B	O�B	N�B	I�B	G�B	F�B	D�B	C�B	?}B	:^B	,B	�B	�B	oB	DB	%B	  B��B��B��B�B�B�fB�BƨB�wB�9B�9B�!B�B�B�B�B��B��B��B��B��B��B�uB�PB�+B�B�B�B~�B|�Bw�Bs�Bo�Bo�Bn�Bm�Bk�BiyBgmBe`B`BB\)BYBXBVBW
BXBXBYBZBXBW
BT�BT�BVBVBT�BT�BT�BT�BS�BS�BS�BR�BR�BQ�BQ�BP�BP�BO�BO�BO�BO�BO�BN�BP�BP�BQ�BT�BYBXBW
BZB[#B\)B[#B\)B[#B]/B]/B^5B^5B^5B^5B_;B_;B_;B_;B_;B^5B^5B_;BaHBbNBaHBbNBbNBaHBbNBffBhsBhsBiyBjBl�Bl�Bl�Bl�Bk�Bl�Bk�BiyBjBm�Bm�Bm�Bm�Bn�Bp�Br�Bt�Bu�Bv�Bw�B�B�%B�7B�JB�VB�PB�hB�uB�{B��B��B��B��B��B�9B�?B�qB�wB�}B�}BƨBɺB��B��B�
B�B�B�B�
B�
B�
B�
B�/B�HB�NB�ZB�`B�sB�B��B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	B	B	B		7B	JB	VB	bB	hB	hB	hB	hB	oB	oB	uB	uB	{B	�B	�B	�B	�B	�B	 �B	"�B	#�B	'�B	)�B	+B	+B	-B	.B	/B	/B	/B	/B	/B	/B	33B	49B	49B	49B	49B	49B	5?B	6FB	6FB	6FB	8RB	;dB	=qB	?}B	?}B	@�B	A�B	C�B	F�B	H�B	I�B	J�B	O�B	VB	XB	\)B	]/B	bNB	gmB	p�B	r�B	x�B	{�B	|�B	}�B	�B	�B	�B	�7B	�DB	�PB	�\B	�hB	�oB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�?B	�?B	�?B	�9B	�9B	�?B	�FB	�LB	�RB	�XB	�XB	�XB	�^B	�^B	�dB	�jB	�jB	�qB	�wB	��B	ĜB	ĜB	ŢB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�#B	�)B	�5B	�;B	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B
	7B
DB
DB
JB
JB
PB
PB
PB
PB
VB
VB
PB
PB
VB
VB
VB
VB
VB
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
 �B
!�B
!�B
 �B
!�B
!�B
!�B
!�B
 �B
 �B
!�B
!�B
!�B
#�B
$�B
$�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
-B
.B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
1'B
2-B
33B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
9XB
9XB
:^B
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
=qB
>wB
=qB
=qB
=qB
=qB
>wB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
?}B
?}B
?}B
@�B
?}B
@�B
@�B
@�B
@�B
@�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
F�B
G�B
I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B  B�BZB~�B}�BjBR�BJ�BB�B>wB;dBO�Br�Bk�BG�B=qB9XBK�Bz�BffB��B�'B�!B�B�B��B��B��B�3B�-B�FB�RB�RB�RB�RB�XB�XB�RB�^BĜBŢBĜBBB��B��B�jB�dB�wB�jB�qB�}B�wB�jB�XB��B��B��B�bB�B� Bz�By�Bt�BhsB`BB^5B^5B\)BXBL�BE�B=qB2-B.B$�BuB1B��B�B�B��B��B��B�LB�B��B��B�bB�%Bz�Bp�BiyBdZBI�B$�B
��B
�yB
�`B
��B
��B
��B
�RB
�B
��B
��B
�B
{�B
p�B
cTB
^5B
YB
P�B
I�B
>wB
0!B
&�B
hB
%B
B	��B	�B	�TB	�B	��B	ĜB	��B	��B	�B	k�B	^5B	XB	W
B	W
B	VB	VB	R�B	P�B	O�B	N�B	I�B	G�B	F�B	D�B	C�B	?}B	:^B	,B	�B	�B	oB	DB	%B	  B��B��B��B�B�B�fB�BƨB�wB�9B�9B�!B�B�B�B�B��B��B��B��B��B��B�uB�PB�+B�B�B�B~�B|�Bw�Bs�Bo�Bo�Bn�Bm�Bk�BiyBgmBe`B`BB\)BYBXBVBW
BXBXBYBZBXBW
BT�BT�BVBVBT�BT�BT�BT�BS�BS�BS�BR�BR�BQ�BQ�BP�BP�BO�BO�BO�BO�BO�BN�BP�BP�BQ�BT�BYBXBW
BZB[#B\)B[#B\)B[#B]/B]/B^5B^5B^5B^5B_;B_;B_;B_;B_;B^5B^5B_;BaHBbNBaHBbNBbNBaHBbNBffBhsBhsBiyBjBl�Bl�Bl�Bl�Bk�Bl�Bk�BiyBjBm�Bm�Bm�Bm�Bn�Bp�Br�Bt�Bu�Bv�Bw�B�B�%B�7B�JB�VB�PB�hB�uB�{B��B��B��B��B��B�9B�?B�qB�wB�}B�}BƨBɺB��B��B�
B�B�B�B�
B�
B�
B�
B�/B�HB�NB�ZB�`B�sB�B��B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	B	B	B		7B	JB	VB	bB	hB	hB	hB	hB	oB	oB	uB	uB	{B	�B	�B	�B	�B	�B	 �B	"�B	#�B	'�B	)�B	+B	+B	-B	.B	/B	/B	/B	/B	/B	/B	33B	49B	49B	49B	49B	49B	5?B	6FB	6FB	6FB	8RB	;dB	=qB	?}B	?}B	@�B	A�B	C�B	F�B	H�B	I�B	J�B	O�B	VB	XB	\)B	]/B	bNB	gmB	p�B	r�B	x�B	{�B	|�B	}�B	�B	�B	�B	�7B	�DB	�PB	�\B	�hB	�oB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�?B	�?B	�?B	�9B	�9B	�?B	�FB	�LB	�RB	�XB	�XB	�XB	�^B	�^B	�dB	�jB	�jB	�qB	�wB	��B	ĜB	ĜB	ŢB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�#B	�)B	�5B	�;B	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B
	7B
DB
DB
JB
JB
PB
PB
PB
PB
VB
VB
PB
PB
VB
VB
VB
VB
VB
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
 �B
!�B
!�B
 �B
!�B
!�B
!�B
!�B
 �B
 �B
!�B
!�B
!�B
#�B
$�B
$�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
-B
.B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
1'B
2-B
33B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
9XB
9XB
:^B
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
=qB
>wB
=qB
=qB
=qB
=qB
>wB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
?}B
?}B
?}B
@�B
?}B
@�B
@�B
@�B
@�B
@�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
F�B
G�B
I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20170928170207                              AO  ARCAADJP                                                                    20170928170207    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170928170207  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170928170207  QCF$                G�O�G�O�G�O�0               
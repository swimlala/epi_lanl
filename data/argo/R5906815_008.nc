CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-08-31T10:01:02Z creation      
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
resolution        =���   axis      Z        H  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  n�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ʨ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220831100102  20220831100102  5906815 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8808                            2B  A   NAVIS_A                         1396                            170425                          863 @��e���1   @����@7+C��%�c�r� Ĝ1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/��D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�@�D��D��]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jA�n�A�`BA�&�AՅA�VA���A��yA��`A��;A��#A��#A��A���A���A��HA���A�  A���A��A���AԬAԉ7A�\)A�hsA�VA�~�A�Q�A�t�AθRA�O�A���Aʙ�A��A���A�hsA��A���A�S�A�A�A�oA��A��\A�  A�n�A���A��A��A��A��
A��RA���A�z�A�"�A��HA�x�A���A��
A��9A���A���A�\)A�
=A���A��A���A�bNA�bNA�x�A�v�A��jA�dZA��A�l�A��9A��DA���A���A��A��A�ĜA��DA�~�A��DA���A��A��hA�ĜA��DA�JA�z�A���A���A�
=A�ȴA�"�A���A��A���A�O�A���A��wA�-A�n�A��-A��jA��HA��A�z�A���A�JA�x�A���A�A�ffA��^A~bNAzA�Ay�Aw��At  Ap9XAo7LAn��AnAk�mAh �Ag7LAf�Ac�;AbI�AaC�A_p�A\1AZ�`AZ-AXE�AW&�AV5?AT��ASO�AQl�AOXAM7LAK�AKt�AJjAI��AHȴAG�AE��AC�A@�`A>~�A=33A;�#A;K�A;�A:ZA9+A6��A6{A5"�A4�uA4�A3��A3;dA2v�A/�PA,�!A,A+�;A+�A*�DA(n�A'��A&9XA#��A"z�A!%A M�A��AA��A�wAƨAv�A��A��Al�A��AA�A�+AK�A�A;dA1A�/A(�A/AQ�At�A
E�A	��A	��A	x�A	G�A��AM�A�`A��A�hA�7Ax�A=qA�DA�AO�A �/A $�@��@�ff@��T@���@�&�@���@�j@�\)@��@��@�C�@�n�@��^@�ƨ@���@�?}@�@�ȴ@�j@�C�@��@�@�G�@�V@�Q�@�n�@�J@�@�%@�dZ@���@�7@���@�9@�V@�Q�@ە�@���@�5?@ّh@�bN@ו�@�"�@��@�-@��/@�dZ@�-@�/@υ@Η�@�X@�z�@˶F@�n�@���@ǝ�@��@Ɵ�@Ų-@ă@Å@�@�X@��@��@��@�A�@�;d@�5?@�x�@��`@���@�S�@���@��@�&�@���@�9X@��m@�ȴ@��@���@�/@�b@��
@�"�@�M�@��-@�&�@���@�r�@�b@�S�@��R@�E�@�-@�{@��@���@��@��@��@�p�@�G�@���@���@�b@���@�"�@��H@�^5@�x�@��u@�Q�@��;@���@�C�@�;d@���@�$�@�@���@��h@�/@��@�O�@�O�@��D@�bN@�j@�r�@�Z@��;@�|�@�t�@��@�^5@�M�@�$�@��T@��-@�p�@��@�1'@��@���@�33@��H@�^5@�$�@���@���@���@�hs@�V@��D@���@�5?@���@�r�@�  @�t�@�\)@�33@�
=@�v�@�-@��^@�hs@�Ĝ@��u@��u@�j@�I�@�(�@�  @�b@��@��m@��@� �@��@�b@��F@��@���@���@���@���@��P@�t�@�|�@�l�@���@�ff@�5?@��@��7@�/@��/@��@�z�@�1'@� �@�  @��
@��P@��@��@���@���@��@���@���@��@��w@��w@��F@��@�S�@�33@��H@���@�ȴ@���@��H@��@�
=@�K�@�S�@�
=@��@���@��R@���@�n�@�E�@�{@��T@��^@�hs@��@���@��`@��j@�j@�I�@�  @�ƨ@�t�@�S�@�C�@�o@��!@�~�@�n�@�5?@��#@���@��h@�`B@�G�@��@�V@�Ĝ@��u@�r�@�A�@�(�@��@���@��F@�\)@�@���@�v�@�ff@�E�@�$�@�J@�@��@��#@���@��h@�`B@�?}@�/@��@�%@��`@���@���@�bN@��@�w@�@~��@~�+@~ff@~5?@~5?@~{@}��@}?}@}�@|�@|(�@{�m@z��@zn�@zJ@y�@y��@y�^@y��@y�7@yhs@yX@y%@xĜ@x�@xA�@x  @w�P@vE�@u�-@uO�@u�@t��@t�/@tZ@t�@s�m@s�
@sƨ@s�F@s��@sC�@so@r^5@q�#@p�`@pbN@p  @o�@o\)@o+@o
=@n�@nff@m��@m�h@mV@l�@l(�@k��@k33@jn�@jJ@i��@i%@hr�@hbN@hQ�@h  @g��@g�P@gl�@f�y@f@e�h@e?}@d��@d�j@d��@dZ@d9X@c��@co@bn�@a��@a�7@ax�@a&�@`��@`�9@`�u@`bN@_�@_\)@_K�@_+@_+@^�y@^�+@^5?@]�-@]V@\�j@\z�@\j@\Z@\I�@[�m@[t�@[C�@Z��@Z~�@Y��@Y7L@Y&�@X��@X�9@Xb@W�;@W�@W��@W|�@W�@V�R@V��@V�+@VV@V@U�-@Up�@UO�@U/@UV@T�@T�j@T��@TZ@S��@S"�@R��@R�\@Rn�@R�@Q��@Q�7@Q%@P�`@P�@P �@O��@O�P@Ol�@N��@N�R@N{@M�@L��@L�@Lz�@L1@K��@Kt�@K@Jn�@J-@I��@IG�@I&�@I%@H��@H�9@H�u@HA�@G�w@GK�@G�@F��@F�@F�R@FV@F@E@E�-@E�@E/@EV@D��@D�D@Dz�@Dz�@Dz�@D�@C��@CC�@B��@B��@B��@B�\@BJ@A�7@A%@@Ĝ@@��@@bN@@ �@@  @?��@?K�@?
=@>ȴ@>�R@>�R@>v�@>ff@>V@>V@>E�@>5?@>{@>@=@=�h@=p�@<��@<�j@<�D@<j@<(�@;��@;�F@;t�@;"�@:�@:�\@:M�@:�@9�^@9�7@9x�@9x�@9X@9&�@8��@8�9@8Q�@8 �@8  @7��@7��@7\)@7
=@6�@6�R@6$�@5�@5�-@5�@5p�@5�@5V@4��@4z�@4I�@49X@4(�@3��@333@2�H@2�!@2�\@1��@1G�@0�`@0r�@0  @/�P@/l�@/;d@.�y@.��@.E�@.5?@.@-@-`B@-�@,��@,j@,1@+��@+dZ@+33@+o@+@*��@*^5@*J@)��@)�^@)G�@(Ĝ@(�9@(�u@(�@(bN@( �@'��@'��@'��@'�P@'+@&��@&�@&��@&V@%��@%`B@%?}@%?}@%V@$�j@$�@$Z@$1@#��@#�F@#t�@#C�@#"�@"��@"��@"~�@"=q@"-@!��@!&�@ ��@ �9@ ��@ ��@ A�@ b@   @�@��@|�@\)@+@��@ȴ@�+@$�@@�T@��@��@��@O�@�j@j@(�@�@�m@�
@�
@ƨ@�F@S�@@�!@~�@=q@J@�@��@�7@G�@�@%@��@Ĝ@�u@r�@Q�@ �@  @�;@��@\)@;d@�@��@�R@v�@E�@�@��@�h@`B@?}@V@�@�/@�j@��@Z@9X@�
@��@S�@C�@33@"�@�H@��@�\@n�@M�@=q@�@�@��@hs@&�@%@�`@Ĝ@Q�@�;@l�@+@�@
=@��@�R@��@�+@V@{@@�T@�h@`B@�@V@�/@��@z�@I�@9X@�
@��@t�@S�@33@"�@o@
�@
�!@
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�jA�n�A�`BA�&�AՅA�VA���A��yA��`A��;A��#A��#A��A���A���A��HA���A�  A���A��A���AԬAԉ7A�\)A�hsA�VA�~�A�Q�A�t�AθRA�O�A���Aʙ�A��A���A�hsA��A���A�S�A�A�A�oA��A��\A�  A�n�A���A��A��A��A��
A��RA���A�z�A�"�A��HA�x�A���A��
A��9A���A���A�\)A�
=A���A��A���A�bNA�bNA�x�A�v�A��jA�dZA��A�l�A��9A��DA���A���A��A��A�ĜA��DA�~�A��DA���A��A��hA�ĜA��DA�JA�z�A���A���A�
=A�ȴA�"�A���A��A���A�O�A���A��wA�-A�n�A��-A��jA��HA��A�z�A���A�JA�x�A���A�A�ffA��^A~bNAzA�Ay�Aw��At  Ap9XAo7LAn��AnAk�mAh �Ag7LAf�Ac�;AbI�AaC�A_p�A\1AZ�`AZ-AXE�AW&�AV5?AT��ASO�AQl�AOXAM7LAK�AKt�AJjAI��AHȴAG�AE��AC�A@�`A>~�A=33A;�#A;K�A;�A:ZA9+A6��A6{A5"�A4�uA4�A3��A3;dA2v�A/�PA,�!A,A+�;A+�A*�DA(n�A'��A&9XA#��A"z�A!%A M�A��AA��A�wAƨAv�A��A��Al�A��AA�A�+AK�A�A;dA1A�/A(�A/AQ�At�A
E�A	��A	��A	x�A	G�A��AM�A�`A��A�hA�7Ax�A=qA�DA�AO�A �/A $�@��@�ff@��T@���@�&�@���@�j@�\)@��@��@�C�@�n�@��^@�ƨ@���@�?}@�@�ȴ@�j@�C�@��@�@�G�@�V@�Q�@�n�@�J@�@�%@�dZ@���@�7@���@�9@�V@�Q�@ە�@���@�5?@ّh@�bN@ו�@�"�@��@�-@��/@�dZ@�-@�/@υ@Η�@�X@�z�@˶F@�n�@���@ǝ�@��@Ɵ�@Ų-@ă@Å@�@�X@��@��@��@�A�@�;d@�5?@�x�@��`@���@�S�@���@��@�&�@���@�9X@��m@�ȴ@��@���@�/@�b@��
@�"�@�M�@��-@�&�@���@�r�@�b@�S�@��R@�E�@�-@�{@��@���@��@��@��@�p�@�G�@���@���@�b@���@�"�@��H@�^5@�x�@��u@�Q�@��;@���@�C�@�;d@���@�$�@�@���@��h@�/@��@�O�@�O�@��D@�bN@�j@�r�@�Z@��;@�|�@�t�@��@�^5@�M�@�$�@��T@��-@�p�@��@�1'@��@���@�33@��H@�^5@�$�@���@���@���@�hs@�V@��D@���@�5?@���@�r�@�  @�t�@�\)@�33@�
=@�v�@�-@��^@�hs@�Ĝ@��u@��u@�j@�I�@�(�@�  @�b@��@��m@��@� �@��@�b@��F@��@���@���@���@���@��P@�t�@�|�@�l�@���@�ff@�5?@��@��7@�/@��/@��@�z�@�1'@� �@�  @��
@��P@��@��@���@���@��@���@���@��@��w@��w@��F@��@�S�@�33@��H@���@�ȴ@���@��H@��@�
=@�K�@�S�@�
=@��@���@��R@���@�n�@�E�@�{@��T@��^@�hs@��@���@��`@��j@�j@�I�@�  @�ƨ@�t�@�S�@�C�@�o@��!@�~�@�n�@�5?@��#@���@��h@�`B@�G�@��@�V@�Ĝ@��u@�r�@�A�@�(�@��@���@��F@�\)@�@���@�v�@�ff@�E�@�$�@�J@�@��@��#@���@��h@�`B@�?}@�/@��@�%@��`@���@���@�bN@��@�w@�@~��@~�+@~ff@~5?@~5?@~{@}��@}?}@}�@|�@|(�@{�m@z��@zn�@zJ@y�@y��@y�^@y��@y�7@yhs@yX@y%@xĜ@x�@xA�@x  @w�P@vE�@u�-@uO�@u�@t��@t�/@tZ@t�@s�m@s�
@sƨ@s�F@s��@sC�@so@r^5@q�#@p�`@pbN@p  @o�@o\)@o+@o
=@n�@nff@m��@m�h@mV@l�@l(�@k��@k33@jn�@jJ@i��@i%@hr�@hbN@hQ�@h  @g��@g�P@gl�@f�y@f@e�h@e?}@d��@d�j@d��@dZ@d9X@c��@co@bn�@a��@a�7@ax�@a&�@`��@`�9@`�u@`bN@_�@_\)@_K�@_+@_+@^�y@^�+@^5?@]�-@]V@\�j@\z�@\j@\Z@\I�@[�m@[t�@[C�@Z��@Z~�@Y��@Y7L@Y&�@X��@X�9@Xb@W�;@W�@W��@W|�@W�@V�R@V��@V�+@VV@V@U�-@Up�@UO�@U/@UV@T�@T�j@T��@TZ@S��@S"�@R��@R�\@Rn�@R�@Q��@Q�7@Q%@P�`@P�@P �@O��@O�P@Ol�@N��@N�R@N{@M�@L��@L�@Lz�@L1@K��@Kt�@K@Jn�@J-@I��@IG�@I&�@I%@H��@H�9@H�u@HA�@G�w@GK�@G�@F��@F�@F�R@FV@F@E@E�-@E�@E/@EV@D��@D�D@Dz�@Dz�@Dz�@D�@C��@CC�@B��@B��@B��@B�\@BJ@A�7@A%@@Ĝ@@��@@bN@@ �@@  @?��@?K�@?
=@>ȴ@>�R@>�R@>v�@>ff@>V@>V@>E�@>5?@>{@>@=@=�h@=p�@<��@<�j@<�D@<j@<(�@;��@;�F@;t�@;"�@:�@:�\@:M�@:�@9�^@9�7@9x�@9x�@9X@9&�@8��@8�9@8Q�@8 �@8  @7��@7��@7\)@7
=@6�@6�R@6$�@5�@5�-@5�@5p�@5�@5V@4��@4z�@4I�@49X@4(�@3��@333@2�H@2�!@2�\@1��@1G�@0�`@0r�@0  @/�P@/l�@/;d@.�y@.��@.E�@.5?@.@-@-`B@-�@,��@,j@,1@+��@+dZ@+33@+o@+@*��@*^5@*J@)��@)�^@)G�@(Ĝ@(�9@(�u@(�@(bN@( �@'��@'��@'��@'�P@'+@&��@&�@&��@&V@%��@%`B@%?}@%?}@%V@$�j@$�@$Z@$1@#��@#�F@#t�@#C�@#"�@"��@"��@"~�@"=q@"-@!��@!&�@ ��@ �9@ ��@ ��@ A�@ b@   @�@��@|�@\)@+@��@ȴ@�+@$�@@�T@��@��@��@O�@�j@j@(�@�@�m@�
@�
@ƨ@�F@S�@@�!@~�@=q@J@�@��@�7@G�@�@%@��@Ĝ@�u@r�@Q�@ �@  @�;@��@\)@;d@�@��@�R@v�@E�@�@��@�h@`B@?}@V@�@�/@�j@��@Z@9X@�
@��@S�@C�@33@"�@�H@��@�\@n�@M�@=q@�@�@��@hs@&�@%@�`@Ĝ@Q�@�;@l�@+@�@
=@��@�R@��@�+@V@{@@�T@�h@`B@�@V@�/@��@z�@I�@9X@�
@��@t�@S�@33@"�@o@
�@
�!@
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�VB�VB�bB��B�B�jB��BBŢBƨBƨBǮBǮBȴB��B��B�B�;B�BB�NB�`B�B�B�B��BVB�B�BoBDB��BoB{B1B�B�)BǮBŢB�B�B�'B�9B�3B�-B�9B�LB�RB�RB�LB�dB�^B�jBɺB�;B�)B�ZB�ZB�mB�`B�ZB�HB�BB�NB�HB�)B�B��B��B��B�FB��B��B��B�\B�%Bo�B`BBS�BC�B9XB!�B�B�B
=B��B�B�B�)BŢB�B��B��B�1Bx�Br�Be`B\)BN�B49B'�BJB
��B
��B
�5B
ĜB
�LB
�B
��B
�1B
}�B
w�B
r�B
hsB
W
B
L�B
7LB
�B	��B	�B	�yB	��B	�^B	�LB	�FB	�FB	��B	�{B	�PB	�=B	}�B	o�B	hsB	^5B	K�B	A�B	>wB	9XB	33B	.B	&�B	�B	uB	+B��B��B��B�B�B�B�NB�B��BB�?B�'B�B��B��B��B��B��B��B��B��B�{B�oB�bB�JB�1B}�Bz�By�Bw�Bv�Bn�Bl�BjBbNB_;B\)BZBZBXBT�BO�BK�BG�BG�BE�BF�BF�BD�BD�BB�B@�B>wB<jB:^B8RB6FB33B33B1'B0!B1'B0!B/B/B,B,B(�B(�B'�B&�B'�B$�B$�B#�B"�B#�B"�B"�B"�B!�B!�B!�B!�B!�B"�B"�B"�B!�B!�B$�B#�B%�B$�B$�B&�B'�B'�B'�B'�B'�B)�B)�B)�B(�B(�B+B,B,B,B+B.B-B,B-B-B.B/B/B/B/B/B1'B1'B49B6FB6FB7LB9XB9XB;dB=qB@�BB�BD�BD�BF�BI�BK�BN�BO�BR�BS�BXB^5B_;BbNBcTBe`BhsBjBm�Bn�Br�Bs�Bt�Bu�By�B{�B|�B~�B�B�B�+B�DB�\B�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�LB�^B�wB�}BĜBǮBȴB��B��B��B��B��B��B�
B�B�#B�5B�HB�`B�yB�B�B�B�B��B��B��B��B��B	B	B	B	B	1B		7B	VB	bB	�B	�B	�B	%�B	(�B	)�B	/B	1'B	1'B	33B	6FB	;dB	;dB	9XB	9XB	:^B	=qB	B�B	C�B	E�B	G�B	L�B	P�B	Q�B	VB	[#B	\)B	]/B	`BB	dZB	gmB	jB	l�B	m�B	n�B	o�B	t�B	v�B	z�B	� B	� B	� B	�B	�B	�B	�%B	�7B	�=B	�JB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�FB	�LB	�XB	�jB	�qB	��B	�}B	��B	B	ÖB	ÖB	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�TB	�TB	�ZB	�`B	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B

=B
DB
DB
JB
JB
JB
JB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
hB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
(�B
(�B
(�B
)�B
)�B
+B
,B
,B
,B
,B
-B
-B
.B
.B
.B
/B
0!B
0!B
0!B
1'B
2-B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
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
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
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
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�VB�VB�bB��B�B�jB��BBŢBƨBƨBǮBǮBȴB��B��B�B�;B�BB�NB�`B�B�B�B��BVB�B�BoBDB��BoB{B1B�B�)BǮBŢB�B�B�'B�9B�3B�-B�9B�LB�RB�RB�LB�dB�^B�jBɺB�;B�)B�ZB�ZB�mB�`B�ZB�HB�BB�NB�HB�)B�B��B��B��B�FB��B��B��B�\B�%Bo�B`BBS�BC�B9XB!�B�B�B
=B��B�B�B�)BŢB�B��B��B�1Bx�Br�Be`B\)BN�B49B'�BJB
��B
��B
�5B
ĜB
�LB
�B
��B
�1B
}�B
w�B
r�B
hsB
W
B
L�B
7LB
�B	��B	�B	�yB	��B	�^B	�LB	�FB	�FB	��B	�{B	�PB	�=B	}�B	o�B	hsB	^5B	K�B	A�B	>wB	9XB	33B	.B	&�B	�B	uB	+B��B��B��B�B�B�B�NB�B��BB�?B�'B�B��B��B��B��B��B��B��B��B�{B�oB�bB�JB�1B}�Bz�By�Bw�Bv�Bn�Bl�BjBbNB_;B\)BZBZBXBT�BO�BK�BG�BG�BE�BF�BF�BD�BD�BB�B@�B>wB<jB:^B8RB6FB33B33B1'B0!B1'B0!B/B/B,B,B(�B(�B'�B&�B'�B$�B$�B#�B"�B#�B"�B"�B"�B!�B!�B!�B!�B!�B"�B"�B"�B!�B!�B$�B#�B%�B$�B$�B&�B'�B'�B'�B'�B'�B)�B)�B)�B(�B(�B+B,B,B,B+B.B-B,B-B-B.B/B/B/B/B/B1'B1'B49B6FB6FB7LB9XB9XB;dB=qB@�BB�BD�BD�BF�BI�BK�BN�BO�BR�BS�BXB^5B_;BbNBcTBe`BhsBjBm�Bn�Br�Bs�Bt�Bu�By�B{�B|�B~�B�B�B�+B�DB�\B�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�LB�^B�wB�}BĜBǮBȴB��B��B��B��B��B��B�
B�B�#B�5B�HB�`B�yB�B�B�B�B��B��B��B��B��B	B	B	B	B	1B		7B	VB	bB	�B	�B	�B	%�B	(�B	)�B	/B	1'B	1'B	33B	6FB	;dB	;dB	9XB	9XB	:^B	=qB	B�B	C�B	E�B	G�B	L�B	P�B	Q�B	VB	[#B	\)B	]/B	`BB	dZB	gmB	jB	l�B	m�B	n�B	o�B	t�B	v�B	z�B	� B	� B	� B	�B	�B	�B	�%B	�7B	�=B	�JB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�FB	�LB	�XB	�jB	�qB	��B	�}B	��B	B	ÖB	ÖB	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�TB	�TB	�ZB	�`B	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B

=B
DB
DB
JB
JB
JB
JB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
hB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
(�B
(�B
(�B
)�B
)�B
+B
,B
,B
,B
,B
-B
-B
.B
.B
.B
/B
0!B
0!B
0!B
1'B
2-B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
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
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
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
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220831100102                              AO  ARCAADJP                                                                    20220831100102    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220831100102  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20220831100102  QCF$                G�O�G�O�G�O�0               
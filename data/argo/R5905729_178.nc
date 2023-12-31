CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-03-12T09:02:20Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  o`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230312090220  20230312090220  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @�VN ��1   @�V�D0@*��	k���d����1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B�\B�\B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\Bè�BǨ�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CN�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�zCg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
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
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֺ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AҁAҁAҁA�~�A�~�A�~�A҅A҃AҁAҁA҅A҉7AҋDA҉7AҋDAҍPAҁA�r�A�bAѮA�+A�ƨA�1'A��yAϴ9AϑhA�r�A�bNA�S�A�I�A�C�A�(�A��A΍PA�dZA�VA�C�A���A��A��A�x�A�^5A�7LA�A��TA��uA�{A�A�O�A��A���A�;dA���A�-A���A���A��!A��+A�A�A���A�dZA�1'A���A���A��mA��A�n�A�
=A�t�A�v�A��HA���A�
=A���A��+A{��Au��Ao��Ak%Ahr�Ae�#AeO�Ac�^A`Q�A]C�AZ�9AY�AX9XAT�`AQ�#AO�AN�\AJ��AH�AG��AG��AF�yAD-A@�HA>r�A;��A:1A81A6�A2JA1�FA2��A4=qA3��A3"�A3oA2�/A3hsA2�`A1�;A0�A0r�A0M�A01'A/��A/��A/�A.�HA-|�A-G�A-dZA-O�A,��A,(�A+�A+33A*JA)7LA't�A&ffA%�A%`BA%\)A$��A$M�A#G�A"ȴA!A ��A�A��AC�A��A�
A��A�RA&�A~�A  A��Ap�AG�A��AAƨA�-A`BA;dA��A �A�A+AjAI�A^5A5?A�A�FA?}A��Av�AE�A�#A�PAG�A�yA��A9XA�Ax�AK�A+A��A�HA�A$�AS�A�A~�AffA-A�;A\)A�A�DAZA  Ax�AdZAXAVA
�/A
ĜA
z�A
1'A	ƨA	t�A	"�A�9A^5A1A�#AAl�A33AoA�A=qA�-A�Al�A%A��AbNA��A�AXA"�A�yA��A��AJA`BA �yA ȴA ��A r�A 1@��@�v�@�$�@��-@��@�b@���@�{@�x�@�z�@�l�@�{@���@�bN@���@�33@�ȴ@�M�@�{@�@���@��m@�;d@���@�^5@�@�%@��
@�@�`B@�9@��@�ȴ@��T@�b@�+@�$�@��
@�ff@���@��@ܣ�@�9X@�ƨ@�"�@��@���@�v�@ف@���@�  @ץ�@�|�@�
=@և+@պ^@�%@ԋD@�bN@��
@�dZ@�+@�o@��y@�5?@�G�@мj@�r�@�1'@�  @υ@�ȴ@��#@̛�@��@��m@˅@�t�@�l�@�o@ʸR@�5?@�{@�@��`@ȣ�@�j@�Q�@�(�@�ƨ@�\)@Ƨ�@��@�hs@��@��/@�j@�(�@��
@�;d@¸R@�v�@�ff@�ff@�M�@�5?@��@�O�@�r�@���@�t�@�o@���@���@�^5@�=q@���@��j@�(�@��@��@��P@�;d@�o@��\@�ff@�E�@�@��-@�p�@���@�1'@�1@���@�K�@��R@�@�@�p�@��@��D@��;@�dZ@���@��T@�`B@��@��@��@��@�t�@�S�@�"�@���@���@�~�@��@��-@�X@��@��`@��u@�9X@��@��P@�
=@��\@�^5@�5?@�J@���@�/@���@��@�1'@�|�@�"�@��+@�-@��@��`@��j@��D@�Q�@�9X@��m@��F@�S�@�"�@���@���@�v�@�$�@���@���@�@�/@��@�Ĝ@���@��u@��D@�z�@�A�@���@��
@�ƨ@��F@���@�t�@�C�@��@��H@��R@��+@�{@��T@��^@�`B@���@���@�r�@��w@�l�@�@���@�ȴ@��+@�$�@���@��-@�V@�Ĝ@��@�|�@�\)@�;d@�33@�+@��y@�ff@���@���@�hs@�V@��/@�1'@���@���@��R@���@�ff@�E�@�{@��@��T@���@��@��@���@�9X@���@�S�@���@��+@�V@�{@��T@��-@��7@�p�@�X@�&�@�%@���@���@�A�@���@���@�S�@�o@��!@��@��@��^@�`B@�G�@�%@��@�  @���@�\)@���@�5?@���@�G�@���@��j@��j@��u@�Z@�Q�@�1'@���@��P@��@���@�E�@��h@�%@��/@�Ĝ@��9@���@��u@��@�1@|�@;d@~�R@~��@~��@~��@~��@~v�@~@}?}@|j@|I�@|I�@|9X@{�m@z-@y&�@x��@x��@xĜ@x�9@x�u@xr�@x  @w�w@w�@w��@w�@v��@vv�@vv�@v5?@u�h@u?}@t��@t��@sS�@r��@r��@r=q@qx�@q�@pbN@o�;@o�w@o�@o|�@o;d@n�y@n��@m�T@mp�@m/@l��@l�D@l1@kdZ@ko@j��@jn�@i�#@i�^@i��@ix�@i7L@h�@g�@gl�@f��@f�@fȴ@f5?@e�h@e?}@eV@d��@d(�@cƨ@c��@c33@b��@a��@a��@a��@a��@ahs@`�`@_�@^�@]�T@]@]�h@]?}@]V@\�@\�@\I�@\�@[�
@[t�@["�@Z��@Z-@Y�7@Y%@XbN@W��@W|�@W;d@V�y@V�@V�R@Vv�@VV@V$�@U��@T�j@T9X@SS�@R�H@Rn�@R-@R-@R-@Q��@Q�@Q��@Q�@P�u@PbN@PQ�@PA�@P1'@P �@O|�@N�@N�+@Nff@N$�@N@M@M��@Mp�@L�@K�m@K��@K@J-@I�#@I%@H�9@H�@HA�@H �@G�;@G�P@G|�@G\)@G;d@G
=@Fȴ@F�+@E�@E�h@D��@D9X@C�m@CS�@C33@C@BM�@A�#@A��@A�^@Ahs@@Ĝ@@r�@@1'@?��@?�w@?�w@?�@?��@?|�@?l�@?K�@?�@>�@>��@>V@=��@=`B@=/@<��@<�D@;�
@;��@;t�@;C�@;o@:�!@:�!@:M�@9��@97L@8r�@7��@7��@7+@6�@6E�@5�T@5�-@5?}@4��@4j@3�
@3dZ@3C�@3"�@3o@2-@17L@1�@0��@0��@0r�@0 �@/�;@/�@/�P@/;d@/�@/
=@.��@.�y@.�@.��@.{@-��@-�-@-�@-/@-�@,�/@,�@,z�@,j@,Z@,(�@+�
@+t�@+o@*�@*��@*��@*�!@*n�@*-@)�#@)X@(�`@(�`@(�`@(��@(bN@(  @'��@'��@'�P@'+@&�@&��@&ff@&5?@&{@&@%�T@%�T@%��@%@%@%@%�-@%�@%?}@%?}@%/@%/@%/@%/@%/@%/@%V@%V@$��@$�@$�j@$��@$�D@$�D@$z�@$9X@$1@#��@"��@"M�@"-@"J@!��@!hs@!hs@!X@!G�@!G�@!G�@!7L@!%@ Ĝ@ �9@ �u@ r�@ r�@ bN@ A�@ b@�w@|�@K�@+@
=@�R@��@v�@V@E�@$�@�@�-@p�@��@�j@��@�D@z�@Z@Z@9X@1@�
@�@S�@@��@^5@M�@-@��@�^@��@�7@x�@X@7L@�@Ĝ@�u@bN@A�@ �@�;@�w@�P@l�@;d@�@
=@�@��@v�@V@E�@$�@�@O�@�/@�@z�@I�@9X@9X@(�@1@1@�
@�@t�@t�@t�@C�@�H@��@��@^5@-@�@��@��@x�@�@��@Ĝ@�9@�@�@�@1'@�@�;@��@��@|�@l�@+@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AҁAҁAҁA�~�A�~�A�~�A҅A҃AҁAҁA҅A҉7AҋDA҉7AҋDAҍPAҁA�r�A�bAѮA�+A�ƨA�1'A��yAϴ9AϑhA�r�A�bNA�S�A�I�A�C�A�(�A��A΍PA�dZA�VA�C�A���A��A��A�x�A�^5A�7LA�A��TA��uA�{A�A�O�A��A���A�;dA���A�-A���A���A��!A��+A�A�A���A�dZA�1'A���A���A��mA��A�n�A�
=A�t�A�v�A��HA���A�
=A���A��+A{��Au��Ao��Ak%Ahr�Ae�#AeO�Ac�^A`Q�A]C�AZ�9AY�AX9XAT�`AQ�#AO�AN�\AJ��AH�AG��AG��AF�yAD-A@�HA>r�A;��A:1A81A6�A2JA1�FA2��A4=qA3��A3"�A3oA2�/A3hsA2�`A1�;A0�A0r�A0M�A01'A/��A/��A/�A.�HA-|�A-G�A-dZA-O�A,��A,(�A+�A+33A*JA)7LA't�A&ffA%�A%`BA%\)A$��A$M�A#G�A"ȴA!A ��A�A��AC�A��A�
A��A�RA&�A~�A  A��Ap�AG�A��AAƨA�-A`BA;dA��A �A�A+AjAI�A^5A5?A�A�FA?}A��Av�AE�A�#A�PAG�A�yA��A9XA�Ax�AK�A+A��A�HA�A$�AS�A�A~�AffA-A�;A\)A�A�DAZA  Ax�AdZAXAVA
�/A
ĜA
z�A
1'A	ƨA	t�A	"�A�9A^5A1A�#AAl�A33AoA�A=qA�-A�Al�A%A��AbNA��A�AXA"�A�yA��A��AJA`BA �yA ȴA ��A r�A 1@��@�v�@�$�@��-@��@�b@���@�{@�x�@�z�@�l�@�{@���@�bN@���@�33@�ȴ@�M�@�{@�@���@��m@�;d@���@�^5@�@�%@��
@�@�`B@�9@��@�ȴ@��T@�b@�+@�$�@��
@�ff@���@��@ܣ�@�9X@�ƨ@�"�@��@���@�v�@ف@���@�  @ץ�@�|�@�
=@և+@պ^@�%@ԋD@�bN@��
@�dZ@�+@�o@��y@�5?@�G�@мj@�r�@�1'@�  @υ@�ȴ@��#@̛�@��@��m@˅@�t�@�l�@�o@ʸR@�5?@�{@�@��`@ȣ�@�j@�Q�@�(�@�ƨ@�\)@Ƨ�@��@�hs@��@��/@�j@�(�@��
@�;d@¸R@�v�@�ff@�ff@�M�@�5?@��@�O�@�r�@���@�t�@�o@���@���@�^5@�=q@���@��j@�(�@��@��@��P@�;d@�o@��\@�ff@�E�@�@��-@�p�@���@�1'@�1@���@�K�@��R@�@�@�p�@��@��D@��;@�dZ@���@��T@�`B@��@��@��@��@�t�@�S�@�"�@���@���@�~�@��@��-@�X@��@��`@��u@�9X@��@��P@�
=@��\@�^5@�5?@�J@���@�/@���@��@�1'@�|�@�"�@��+@�-@��@��`@��j@��D@�Q�@�9X@��m@��F@�S�@�"�@���@���@�v�@�$�@���@���@�@�/@��@�Ĝ@���@��u@��D@�z�@�A�@���@��
@�ƨ@��F@���@�t�@�C�@��@��H@��R@��+@�{@��T@��^@�`B@���@���@�r�@��w@�l�@�@���@�ȴ@��+@�$�@���@��-@�V@�Ĝ@��@�|�@�\)@�;d@�33@�+@��y@�ff@���@���@�hs@�V@��/@�1'@���@���@��R@���@�ff@�E�@�{@��@��T@���@��@��@���@�9X@���@�S�@���@��+@�V@�{@��T@��-@��7@�p�@�X@�&�@�%@���@���@�A�@���@���@�S�@�o@��!@��@��@��^@�`B@�G�@�%@��@�  @���@�\)@���@�5?@���@�G�@���@��j@��j@��u@�Z@�Q�@�1'@���@��P@��@���@�E�@��h@�%@��/@�Ĝ@��9@���@��u@��@�1@|�@;d@~�R@~��@~��@~��@~��@~v�@~@}?}@|j@|I�@|I�@|9X@{�m@z-@y&�@x��@x��@xĜ@x�9@x�u@xr�@x  @w�w@w�@w��@w�@v��@vv�@vv�@v5?@u�h@u?}@t��@t��@sS�@r��@r��@r=q@qx�@q�@pbN@o�;@o�w@o�@o|�@o;d@n�y@n��@m�T@mp�@m/@l��@l�D@l1@kdZ@ko@j��@jn�@i�#@i�^@i��@ix�@i7L@h�@g�@gl�@f��@f�@fȴ@f5?@e�h@e?}@eV@d��@d(�@cƨ@c��@c33@b��@a��@a��@a��@a��@ahs@`�`@_�@^�@]�T@]@]�h@]?}@]V@\�@\�@\I�@\�@[�
@[t�@["�@Z��@Z-@Y�7@Y%@XbN@W��@W|�@W;d@V�y@V�@V�R@Vv�@VV@V$�@U��@T�j@T9X@SS�@R�H@Rn�@R-@R-@R-@Q��@Q�@Q��@Q�@P�u@PbN@PQ�@PA�@P1'@P �@O|�@N�@N�+@Nff@N$�@N@M@M��@Mp�@L�@K�m@K��@K@J-@I�#@I%@H�9@H�@HA�@H �@G�;@G�P@G|�@G\)@G;d@G
=@Fȴ@F�+@E�@E�h@D��@D9X@C�m@CS�@C33@C@BM�@A�#@A��@A�^@Ahs@@Ĝ@@r�@@1'@?��@?�w@?�w@?�@?��@?|�@?l�@?K�@?�@>�@>��@>V@=��@=`B@=/@<��@<�D@;�
@;��@;t�@;C�@;o@:�!@:�!@:M�@9��@97L@8r�@7��@7��@7+@6�@6E�@5�T@5�-@5?}@4��@4j@3�
@3dZ@3C�@3"�@3o@2-@17L@1�@0��@0��@0r�@0 �@/�;@/�@/�P@/;d@/�@/
=@.��@.�y@.�@.��@.{@-��@-�-@-�@-/@-�@,�/@,�@,z�@,j@,Z@,(�@+�
@+t�@+o@*�@*��@*��@*�!@*n�@*-@)�#@)X@(�`@(�`@(�`@(��@(bN@(  @'��@'��@'�P@'+@&�@&��@&ff@&5?@&{@&@%�T@%�T@%��@%@%@%@%�-@%�@%?}@%?}@%/@%/@%/@%/@%/@%/@%V@%V@$��@$�@$�j@$��@$�D@$�D@$z�@$9X@$1@#��@"��@"M�@"-@"J@!��@!hs@!hs@!X@!G�@!G�@!G�@!7L@!%@ Ĝ@ �9@ �u@ r�@ r�@ bN@ A�@ b@�w@|�@K�@+@
=@�R@��@v�@V@E�@$�@�@�-@p�@��@�j@��@�D@z�@Z@Z@9X@1@�
@�@S�@@��@^5@M�@-@��@�^@��@�7@x�@X@7L@�@Ĝ@�u@bN@A�@ �@�;@�w@�P@l�@;d@�@
=@�@��@v�@V@E�@$�@�@O�@�/@�@z�@I�@9X@9X@(�@1@1@�
@�@t�@t�@t�@C�@�H@��@��@^5@-@�@��@��@x�@�@��@Ĝ@�9@�@�@�@1'@�@�;@��@��@|�@l�@+@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
VB
VB
VB
VB
VB
\B
VB
VB
VB
VB
\B
\B
\B
bB
\B
PB
\B
hB
$�B
0!B
E�B
I�B
M�B
R�B
S�B
VB
XB
[#B
`BB
cTB
bNB
ffB
�1B
��B
�XB
�B
��B>wB�FBuB�B��B�B  B0!B8RB,B{B�B=qB�B1'B"�BPB�B�mB��B�ZB��B�^B�oBx�B?}B �B
��B
�
B
ĜB
�FB
��B
��B
�uB
�B
VB
$�B	�B	�B	�'B	��B	�VB	�+B	y�B	�B	m�B	O�B	G�B	D�B	G�B	<jB	"�B	�B	!�B	"�B	DB	�B	&�B	+B	�B	+B	B	�B	�B	49B	-B	,B	�B	.B	`BB	�=B	�-B	��B	ƨB	ȴB	�B	�#B	�B	�/B	�sB	�B	�B	�B	�sB	�fB	�HB	�BB	�B	��B
B
B
	7B
hB
oB
PB
bB
DB
�B
.B
9XB
C�B
@�B
;dB
=qB
;dB
7LB
1'B
9XB
6FB
G�B
G�B
@�B
7LB
A�B
G�B
N�B
Q�B
T�B
W
B
XB
VB
R�B
W
B
[#B
ZB
\)B
[#B
ZB
ZB
^5B
ZB
^5B
ffB
dZB
bNB
bNB
_;B
`BB
^5B
bNB
_;B
`BB
aHB
_;B
`BB
^5B
^5B
^5B
`BB
aHB
`BB
_;B
ZB
YB
S�B
ZB
YB
]/B
[#B
ZB
XB
YB
W
B
YB
XB
T�B
YB
YB
W
B
XB
XB
W
B
VB
S�B
R�B
R�B
P�B
P�B
Q�B
R�B
R�B
O�B
O�B
O�B
M�B
I�B
J�B
M�B
N�B
J�B
I�B
K�B
H�B
G�B
J�B
I�B
H�B
H�B
E�B
?}B
=qB
?}B
B�B
A�B
>wB
<jB
=qB
:^B
>wB
<jB
9XB
7LB
2-B
8RB
5?B
1'B
0!B
.B
/B
1'B
1'B
0!B
1'B
0!B
1'B
.B
,B
+B
)�B
,B
+B
(�B
"�B
�B
�B
!�B
 �B
�B
�B
�B
uB
�B
oB

=B
JB
\B
hB
uB
uB
oB
hB
uB
�B
{B
bB
bB
hB
uB
uB
hB
VB
PB
JB
JB
\B
PB
VB
bB
bB
\B
JB
DB
JB
bB
bB
bB
VB
JB
JB
VB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
!�B
!�B
!�B
!�B
 �B
�B
!�B
 �B
"�B
"�B
"�B
&�B
&�B
&�B
'�B
&�B
'�B
&�B
(�B
)�B
)�B
(�B
(�B
+B
+B
+B
(�B
+B
-B
-B
-B
-B
-B
,B
+B
-B
.B
.B
.B
-B
-B
-B
-B
-B
-B
,B
.B
.B
-B
,B
.B
.B
,B
/B
/B
0!B
1'B
0!B
/B
0!B
0!B
-B
/B
.B
/B
33B
33B
49B
33B
1'B
/B
1'B
33B
2-B
1'B
2-B
0!B
0!B
2-B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
6FB
49B
6FB
6FB
6FB
8RB
8RB
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
<jB
=qB
=qB
<jB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
?}B
?}B
>wB
?}B
>wB
=qB
<jB
>wB
=qB
<jB
@�B
@�B
B�B
D�B
E�B
F�B
E�B
E�B
F�B
E�B
C�B
A�B
@�B
B�B
B�B
C�B
E�B
I�B
J�B
J�B
J�B
J�B
J�B
H�B
I�B
K�B
K�B
M�B
M�B
M�B
M�B
L�B
J�B
J�B
J�B
M�B
N�B
M�B
K�B
H�B
K�B
O�B
P�B
P�B
P�B
P�B
P�B
O�B
P�B
Q�B
P�B
O�B
O�B
Q�B
Q�B
P�B
O�B
P�B
Q�B
P�B
N�B
Q�B
R�B
Q�B
Q�B
R�B
Q�B
R�B
T�B
VB
T�B
T�B
S�B
S�B
R�B
S�B
T�B
T�B
VB
T�B
T�B
VB
W
B
W
B
W
B
YB
ZB
YB
XB
XB
XB
YB
ZB
[#B
[#B
YB
YB
[#B
\)B
[#B
ZB
[#B
\)B
[#B
[#B
[#B
]/B
^5B
^5B
]/B
\)B
ZB
\)B
\)B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
_;B
_;B
_;B
_;B
_;B
bNB
bNB
aHB
cTB
bNB
bNB
bNB
bNB
aHB
`BB
aHB
aHB
cTB
cTB
e`B
ffB
e`B
e`B
e`B
dZB
cTB
dZB
e`B
ffB
ffB
e`B
e`B
cTB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
e`B
dZB
cTB
ffB
e`B
e`B
gmB
ffB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
iyB
iyB
hsB
iyB
hsB
hsB
iyB
iyB
jB
jB
iyB
jB
m�B
l�B
l�B
k�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
k�B
m�B
m�B
m�B
m�B
l�B
n�B
n�B
n�B
n�B
n�B
p�B
o�B
n�B
n�B
o�B
p�B
q�B
q�B
q�B
q�B
r�B
s�B
r�B
r�B
s�B
s�B
t�B
u�B
u�B
t�B
r�B
q�B
w�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
w�B
y�B
y�B
y�B
y�B
x�B
x�B
w�B
x�B
y�B
y�B
y�B
z�B
y�B
y�B
z�B
z�B
z�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
z�B
z�B
y�B
y�B
z�B
|�B
|�B
{�B
z�B
z�B
{�B
|�B
|�B
{�B
|�B
|�B
}�B
}�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
~�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
� B
�B
�B
� B
~�B
~�B
}�B
{�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�B
�B
�B
�B
�%B
�B
�%B
�%B
�1B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�=B
�=B
�7B
�DB
�JB
�JB
�PB
�VB
�VB
�VB
�PB
�VB
�PB
�PB
�\B
�\B
�\B
�VB
�PB
�\B
�VB
�\B
�\B
�\B
�bB
�bB
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�hB
�bB
�bB
�hB
�hB
�hB
�hB
�oB
�hB
�oB
�o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
VB
VB
VB
VB
VB
\B
VB
VB
VB
VB
\B
\B
\B
bB
\B
PB
\B
hB
$�B
0!B
E�B
I�B
M�B
R�B
S�B
VB
XB
[#B
`BB
cTB
bNB
ffB
�1B
��B
�XB
�B
��B>wB�FBuB�B��B�B  B0!B8RB,B{B�B=qB�B1'B"�BPB�B�mB��B�ZB��B�^B�oBx�B?}B �B
��B
�
B
ĜB
�FB
��B
��B
�uB
�B
VB
$�B	�B	�B	�'B	��B	�VB	�+B	y�B	�B	m�B	O�B	G�B	D�B	G�B	<jB	"�B	�B	!�B	"�B	DB	�B	&�B	+B	�B	+B	B	�B	�B	49B	-B	,B	�B	.B	`BB	�=B	�-B	��B	ƨB	ȴB	�B	�#B	�B	�/B	�sB	�B	�B	�B	�sB	�fB	�HB	�BB	�B	��B
B
B
	7B
hB
oB
PB
bB
DB
�B
.B
9XB
C�B
@�B
;dB
=qB
;dB
7LB
1'B
9XB
6FB
G�B
G�B
@�B
7LB
A�B
G�B
N�B
Q�B
T�B
W
B
XB
VB
R�B
W
B
[#B
ZB
\)B
[#B
ZB
ZB
^5B
ZB
^5B
ffB
dZB
bNB
bNB
_;B
`BB
^5B
bNB
_;B
`BB
aHB
_;B
`BB
^5B
^5B
^5B
`BB
aHB
`BB
_;B
ZB
YB
S�B
ZB
YB
]/B
[#B
ZB
XB
YB
W
B
YB
XB
T�B
YB
YB
W
B
XB
XB
W
B
VB
S�B
R�B
R�B
P�B
P�B
Q�B
R�B
R�B
O�B
O�B
O�B
M�B
I�B
J�B
M�B
N�B
J�B
I�B
K�B
H�B
G�B
J�B
I�B
H�B
H�B
E�B
?}B
=qB
?}B
B�B
A�B
>wB
<jB
=qB
:^B
>wB
<jB
9XB
7LB
2-B
8RB
5?B
1'B
0!B
.B
/B
1'B
1'B
0!B
1'B
0!B
1'B
.B
,B
+B
)�B
,B
+B
(�B
"�B
�B
�B
!�B
 �B
�B
�B
�B
uB
�B
oB

=B
JB
\B
hB
uB
uB
oB
hB
uB
�B
{B
bB
bB
hB
uB
uB
hB
VB
PB
JB
JB
\B
PB
VB
bB
bB
\B
JB
DB
JB
bB
bB
bB
VB
JB
JB
VB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
!�B
!�B
!�B
!�B
 �B
�B
!�B
 �B
"�B
"�B
"�B
&�B
&�B
&�B
'�B
&�B
'�B
&�B
(�B
)�B
)�B
(�B
(�B
+B
+B
+B
(�B
+B
-B
-B
-B
-B
-B
,B
+B
-B
.B
.B
.B
-B
-B
-B
-B
-B
-B
,B
.B
.B
-B
,B
.B
.B
,B
/B
/B
0!B
1'B
0!B
/B
0!B
0!B
-B
/B
.B
/B
33B
33B
49B
33B
1'B
/B
1'B
33B
2-B
1'B
2-B
0!B
0!B
2-B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
6FB
49B
6FB
6FB
6FB
8RB
8RB
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
<jB
=qB
=qB
<jB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
?}B
?}B
>wB
?}B
>wB
=qB
<jB
>wB
=qB
<jB
@�B
@�B
B�B
D�B
E�B
F�B
E�B
E�B
F�B
E�B
C�B
A�B
@�B
B�B
B�B
C�B
E�B
I�B
J�B
J�B
J�B
J�B
J�B
H�B
I�B
K�B
K�B
M�B
M�B
M�B
M�B
L�B
J�B
J�B
J�B
M�B
N�B
M�B
K�B
H�B
K�B
O�B
P�B
P�B
P�B
P�B
P�B
O�B
P�B
Q�B
P�B
O�B
O�B
Q�B
Q�B
P�B
O�B
P�B
Q�B
P�B
N�B
Q�B
R�B
Q�B
Q�B
R�B
Q�B
R�B
T�B
VB
T�B
T�B
S�B
S�B
R�B
S�B
T�B
T�B
VB
T�B
T�B
VB
W
B
W
B
W
B
YB
ZB
YB
XB
XB
XB
YB
ZB
[#B
[#B
YB
YB
[#B
\)B
[#B
ZB
[#B
\)B
[#B
[#B
[#B
]/B
^5B
^5B
]/B
\)B
ZB
\)B
\)B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
_;B
_;B
_;B
_;B
_;B
bNB
bNB
aHB
cTB
bNB
bNB
bNB
bNB
aHB
`BB
aHB
aHB
cTB
cTB
e`B
ffB
e`B
e`B
e`B
dZB
cTB
dZB
e`B
ffB
ffB
e`B
e`B
cTB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
e`B
dZB
cTB
ffB
e`B
e`B
gmB
ffB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
iyB
iyB
hsB
iyB
hsB
hsB
iyB
iyB
jB
jB
iyB
jB
m�B
l�B
l�B
k�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
k�B
m�B
m�B
m�B
m�B
l�B
n�B
n�B
n�B
n�B
n�B
p�B
o�B
n�B
n�B
o�B
p�B
q�B
q�B
q�B
q�B
r�B
s�B
r�B
r�B
s�B
s�B
t�B
u�B
u�B
t�B
r�B
q�B
w�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
w�B
y�B
y�B
y�B
y�B
x�B
x�B
w�B
x�B
y�B
y�B
y�B
z�B
y�B
y�B
z�B
z�B
z�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
z�B
z�B
y�B
y�B
z�B
|�B
|�B
{�B
z�B
z�B
{�B
|�B
|�B
{�B
|�B
|�B
}�B
}�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
~�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
� B
�B
�B
� B
~�B
~�B
}�B
{�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�B
�B
�B
�B
�%B
�B
�%B
�%B
�1B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�=B
�=B
�7B
�DB
�JB
�JB
�PB
�VB
�VB
�VB
�PB
�VB
�PB
�PB
�\B
�\B
�\B
�VB
�PB
�\B
�VB
�\B
�\B
�\B
�bB
�bB
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�hB
�bB
�bB
�hB
�hB
�hB
�hB
�oB
�hB
�oB
�o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230312090220                              AO  ARCAADJP                                                                    20230312090220    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230312090220  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230312090220  QCF$                G�O�G�O�G�O�0               
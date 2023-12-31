CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-02-18T10:29:41Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20210218102941  20210218102941  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               aA   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @�_V|�41   @�_W�lV@;�+I��c��x���1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         aA   A   F   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӃ3D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�z�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�DӀ�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�@�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��wA�A���A���A��
A��A��
A��#A��#A��/A��/A��/A��;A��;A��TA��HA��HA��TA��TA��mA��`A��`A��`A��`A��mA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A���A���A���A��A���A��A�bNA�A�VA�dZA�"�A�VA�ZA�^5A�p�A��A��A�  A���A���A���A�?}A�M�A���A�E�A��/A���A�S�A��jA��A���A�9XA��PA�7LA�^5A�ȴA�jA�"�A���A�7LA�A�hsA��A��A��A���A��mA��hA}��A{��Az�Ay�Au�As�-Aqx�Ap�\Ap=qAp1Ao�An5?Akp�Aj9XAh�jAg�Ac�;AaXA_�-A]K�A[�-A[�AY�AV��AU�TAT�ASS�AQ%AP1'AO&�AMt�AK��AJ��AI7LAG��AF�/AF�\AF1'AE�wAE"�ADQ�AB��AA�
AA?}A@�A>ĜA=&�A;��A;%A:{A9ƨA9ƨA9ƨA8��A8(�A7�-A6��A6^5A6E�A5��A4�!A3C�A2�HA0�A/��A/oA.�jA.~�A.Q�A-��A,��A,1'A+��A+��A*�`A*�uA*9XA)�A)�-A)�A(I�A'�A&��A&$�A%t�A%�A#�A"v�A!;dA ��A��AƨA&�AĜA�A�A��AA=qAƨAr�A�^AK�A��Av�At�Az�A�AA�A��A�FA�A��AM�A
A�A	hsA	O�A	+A	
=A��AVAA�^A�/AhsAȴA�-AjAA �A n�@��P@�G�@��@��9@��P@�hs@�dZ@��!@�Z@��@�@�^5@��@�@���@���@�j@�Q�@���@�  @��@��@�;d@��^@�o@�Z@�t�@�1'@ڏ\@�V@��/@ؼj@��@���@��@�Q�@��y@с@��/@�j@϶F@Ο�@�bN@�|�@��T@��m@��y@Ə\@�5?@őh@�?}@ċD@�b@�K�@�@���@���@��
@�ȴ@���@��7@��u@�"�@��/@���@�M�@��@��u@���@�l�@�33@��@�~�@�O�@��D@�1@��w@�C�@�ff@�x�@��`@�Z@�1@��@�S�@��R@�ff@�=q@��@��j@�A�@�33@���@��@��-@��7@�Ĝ@�C�@�+@��y@��#@��@���@���@��P@�t�@�"�@���@���@��!@���@�E�@�J@���@���@�O�@�z�@�l�@���@���@�ff@�$�@��h@�`B@�%@���@���@���@���@�Ĝ@�Q�@�  @���@���@�ȴ@���@���@��D@�r�@�bN@�9X@�S�@��@�E�@�@��7@�Ĝ@�A�@�ƨ@�|�@�S�@�"�@���@��R@�V@�@�x�@�X@�/@���@�  @���@��F@���@�S�@��y@��@�
=@���@�^5@���@�?}@�&�@�V@��@��m@���@���@��P@��@�t�@�\)@�o@���@�ff@�V@�E�@�=q@�$�@��#@�x�@�G�@�?}@��@���@�9X@�t�@�dZ@�S�@�33@�o@��!@�n�@�=q@��@��7@�?}@�/@�7L@�%@��/@��j@��D@�Q�@��@\)@~�R@~v�@~@}?}@|z�@|(�@|z�@|j@|�@{��@{�
@{�F@{��@{�F@{ƨ@{�
@{t�@z�@z�\@z^5@z~�@z��@zn�@y�^@x�u@w�w@vȴ@v�+@v{@u�T@uV@t(�@s�m@r��@r=q@q�#@q�@q�7@qhs@q�@pr�@pQ�@o��@n��@n$�@m@mp�@m`B@m?}@mV@l�@l(�@k�@k@j�\@j-@jJ@i�^@h�`@hQ�@h �@g��@g;d@fȴ@e@e�@ep�@d��@d9X@c��@c��@c��@c��@ct�@c"�@b��@bn�@bn�@b^5@bM�@bM�@a�@a��@ax�@aG�@`�`@_��@_+@_
=@^ȴ@^��@^$�@]@]�h@]�@]p�@]/@\�@\z�@[�m@[��@[dZ@[33@ZM�@Y��@Y��@YX@Y�@X��@X�`@X�`@Y%@X�`@X�@XA�@X �@W�@W�w@W�P@W+@V��@V5?@U��@Up�@T�/@T�D@T9X@S��@Sƨ@S��@St�@R��@R��@R��@R�\@R��@R��@Rn�@Q��@Qhs@QX@QG�@P��@P �@O��@O\)@OK�@Nȴ@N��@N��@N�+@NV@N$�@N{@M@MO�@L��@L(�@K��@Kt�@Kt�@KC�@J��@J�\@Jn�@Jn�@J^5@J=q@J�@I��@JJ@JJ@JJ@I��@I�@I�7@Ihs@IG�@I�@HĜ@H�9@HA�@Hb@H  @G�@G�;@G�w@G��@GK�@F�@F�+@E�T@E��@E��@E�@D��@D��@DZ@D�@C�m@C��@C33@B�@B�\@B=q@A��@A��@A�7@AG�@A%@@�u@@Q�@@ �@?��@?��@?�w@?��@?�P@?|�@?l�@?l�@?�@>�+@>v�@>E�@=�@=@=�-@=��@=�@=O�@<�/@<��@<Z@<9X@<1@;ƨ@;o@:n�@9��@9x�@97L@9�@9%@8��@8��@8bN@8A�@81'@7��@7�@6�@6�R@6�+@6ff@6V@6V@65?@5�@5O�@4�@4�@4Z@49X@4(�@3��@3"�@2��@2��@2^5@2-@1�^@17L@0��@0�9@0��@0�u@0�@0�@0A�@/�w@/\)@.��@.��@.E�@.$�@.{@.@-@-p�@-�@,��@,�/@,�j@,�D@,Z@,�@+ƨ@+dZ@+33@+@*�H@*^5@*^5@*^5@*=q@)��@)X@)7L@)%@(��@(��@(�u@(Q�@'�;@'�@'�P@';d@&�y@&��@&ff@%�@%@%�h@%O�@$��@$�@$z�@$�@#ƨ@#ƨ@#�F@#��@#C�@#@"��@"�!@"~�@"�@!��@!�@!�#@!hs@!&�@!�@!%@ �`@ ��@  �@�w@��@K�@�@�@ȴ@��@v�@V@@�-@�h@O�@�@V@��@�/@�D@j@Z@(�@�@��@�F@33@��@n�@-@J@J@��@X@&�@�@��@�9@��@�u@Q�@b@�;@�@�P@
=@
=@ȴ@��@�+@5?@{@@@�@��@�-@�h@p�@`B@/@��@�/@�/@�/@�j@��@I�@�@��@�
@��@t�@S�@"�@o@��@�\@M�@-@J@�#@��@�7@hs@G�@&�@&�@�@�`@Ĝ@�u@bN@  @  @  @�@��@��@\)@K�@K�@;d@�@
=@
=@
=@ȴ@ff@5?@$�@{@��@�h@�h@�@�@p�@p�@`B@`B@O�@?}@�/@��@j@j@j@j@Z@j@Z@Z@Z@9X@�@1@��@ƨ@�F@�F@��@dZ@"�@@
�@
�H@
�!@
��@
�\@
�\@
~�@
n�@
n�@
^5@
-@	��@	��@	hs@	G�@	&�@	%@��@Ĝ@��@�@Q�@1'@  @�w@�P@\)@+@
=@�R@��@��@�+@v�@ff@ff@E�@5?@@�T@�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��wA�A���A���A��
A��A��
A��#A��#A��/A��/A��/A��;A��;A��TA��HA��HA��TA��TA��mA��`A��`A��`A��`A��mA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A���A���A���A��A���A��A�bNA�A�VA�dZA�"�A�VA�ZA�^5A�p�A��A��A�  A���A���A���A�?}A�M�A���A�E�A��/A���A�S�A��jA��A���A�9XA��PA�7LA�^5A�ȴA�jA�"�A���A�7LA�A�hsA��A��A��A���A��mA��hA}��A{��Az�Ay�Au�As�-Aqx�Ap�\Ap=qAp1Ao�An5?Akp�Aj9XAh�jAg�Ac�;AaXA_�-A]K�A[�-A[�AY�AV��AU�TAT�ASS�AQ%AP1'AO&�AMt�AK��AJ��AI7LAG��AF�/AF�\AF1'AE�wAE"�ADQ�AB��AA�
AA?}A@�A>ĜA=&�A;��A;%A:{A9ƨA9ƨA9ƨA8��A8(�A7�-A6��A6^5A6E�A5��A4�!A3C�A2�HA0�A/��A/oA.�jA.~�A.Q�A-��A,��A,1'A+��A+��A*�`A*�uA*9XA)�A)�-A)�A(I�A'�A&��A&$�A%t�A%�A#�A"v�A!;dA ��A��AƨA&�AĜA�A�A��AA=qAƨAr�A�^AK�A��Av�At�Az�A�AA�A��A�FA�A��AM�A
A�A	hsA	O�A	+A	
=A��AVAA�^A�/AhsAȴA�-AjAA �A n�@��P@�G�@��@��9@��P@�hs@�dZ@��!@�Z@��@�@�^5@��@�@���@���@�j@�Q�@���@�  @��@��@�;d@��^@�o@�Z@�t�@�1'@ڏ\@�V@��/@ؼj@��@���@��@�Q�@��y@с@��/@�j@϶F@Ο�@�bN@�|�@��T@��m@��y@Ə\@�5?@őh@�?}@ċD@�b@�K�@�@���@���@��
@�ȴ@���@��7@��u@�"�@��/@���@�M�@��@��u@���@�l�@�33@��@�~�@�O�@��D@�1@��w@�C�@�ff@�x�@��`@�Z@�1@��@�S�@��R@�ff@�=q@��@��j@�A�@�33@���@��@��-@��7@�Ĝ@�C�@�+@��y@��#@��@���@���@��P@�t�@�"�@���@���@��!@���@�E�@�J@���@���@�O�@�z�@�l�@���@���@�ff@�$�@��h@�`B@�%@���@���@���@���@�Ĝ@�Q�@�  @���@���@�ȴ@���@���@��D@�r�@�bN@�9X@�S�@��@�E�@�@��7@�Ĝ@�A�@�ƨ@�|�@�S�@�"�@���@��R@�V@�@�x�@�X@�/@���@�  @���@��F@���@�S�@��y@��@�
=@���@�^5@���@�?}@�&�@�V@��@��m@���@���@��P@��@�t�@�\)@�o@���@�ff@�V@�E�@�=q@�$�@��#@�x�@�G�@�?}@��@���@�9X@�t�@�dZ@�S�@�33@�o@��!@�n�@�=q@��@��7@�?}@�/@�7L@�%@��/@��j@��D@�Q�@��@\)@~�R@~v�@~@}?}@|z�@|(�@|z�@|j@|�@{��@{�
@{�F@{��@{�F@{ƨ@{�
@{t�@z�@z�\@z^5@z~�@z��@zn�@y�^@x�u@w�w@vȴ@v�+@v{@u�T@uV@t(�@s�m@r��@r=q@q�#@q�@q�7@qhs@q�@pr�@pQ�@o��@n��@n$�@m@mp�@m`B@m?}@mV@l�@l(�@k�@k@j�\@j-@jJ@i�^@h�`@hQ�@h �@g��@g;d@fȴ@e@e�@ep�@d��@d9X@c��@c��@c��@c��@ct�@c"�@b��@bn�@bn�@b^5@bM�@bM�@a�@a��@ax�@aG�@`�`@_��@_+@_
=@^ȴ@^��@^$�@]@]�h@]�@]p�@]/@\�@\z�@[�m@[��@[dZ@[33@ZM�@Y��@Y��@YX@Y�@X��@X�`@X�`@Y%@X�`@X�@XA�@X �@W�@W�w@W�P@W+@V��@V5?@U��@Up�@T�/@T�D@T9X@S��@Sƨ@S��@St�@R��@R��@R��@R�\@R��@R��@Rn�@Q��@Qhs@QX@QG�@P��@P �@O��@O\)@OK�@Nȴ@N��@N��@N�+@NV@N$�@N{@M@MO�@L��@L(�@K��@Kt�@Kt�@KC�@J��@J�\@Jn�@Jn�@J^5@J=q@J�@I��@JJ@JJ@JJ@I��@I�@I�7@Ihs@IG�@I�@HĜ@H�9@HA�@Hb@H  @G�@G�;@G�w@G��@GK�@F�@F�+@E�T@E��@E��@E�@D��@D��@DZ@D�@C�m@C��@C33@B�@B�\@B=q@A��@A��@A�7@AG�@A%@@�u@@Q�@@ �@?��@?��@?�w@?��@?�P@?|�@?l�@?l�@?�@>�+@>v�@>E�@=�@=@=�-@=��@=�@=O�@<�/@<��@<Z@<9X@<1@;ƨ@;o@:n�@9��@9x�@97L@9�@9%@8��@8��@8bN@8A�@81'@7��@7�@6�@6�R@6�+@6ff@6V@6V@65?@5�@5O�@4�@4�@4Z@49X@4(�@3��@3"�@2��@2��@2^5@2-@1�^@17L@0��@0�9@0��@0�u@0�@0�@0A�@/�w@/\)@.��@.��@.E�@.$�@.{@.@-@-p�@-�@,��@,�/@,�j@,�D@,Z@,�@+ƨ@+dZ@+33@+@*�H@*^5@*^5@*^5@*=q@)��@)X@)7L@)%@(��@(��@(�u@(Q�@'�;@'�@'�P@';d@&�y@&��@&ff@%�@%@%�h@%O�@$��@$�@$z�@$�@#ƨ@#ƨ@#�F@#��@#C�@#@"��@"�!@"~�@"�@!��@!�@!�#@!hs@!&�@!�@!%@ �`@ ��@  �@�w@��@K�@�@�@ȴ@��@v�@V@@�-@�h@O�@�@V@��@�/@�D@j@Z@(�@�@��@�F@33@��@n�@-@J@J@��@X@&�@�@��@�9@��@�u@Q�@b@�;@�@�P@
=@
=@ȴ@��@�+@5?@{@@@�@��@�-@�h@p�@`B@/@��@�/@�/@�/@�j@��@I�@�@��@�
@��@t�@S�@"�@o@��@�\@M�@-@J@�#@��@�7@hs@G�@&�@&�@�@�`@Ĝ@�u@bN@  @  @  @�@��@��@\)@K�@K�@;d@�@
=@
=@
=@ȴ@ff@5?@$�@{@��@�h@�h@�@�@p�@p�@`B@`B@O�@?}@�/@��@j@j@j@j@Z@j@Z@Z@Z@9X@�@1@��@ƨ@�F@�F@��@dZ@"�@@
�@
�H@
�!@
��@
�\@
�\@
~�@
n�@
n�@
^5@
-@	��@	��@	hs@	G�@	&�@	%@��@Ĝ@��@�@Q�@1'@  @�w@�P@\)@+@
=@�R@��@��@�+@v�@ff@ff@E�@5?@@�T@�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BB�B�NB��B�jB��B�oB�DB� B{�BffB\)BJ�B9XB.B�BoB+BB��B�fB��B�^B�'B�B��B��B�7B}�Bv�Bq�Bm�BYBN�B;dB%�B�BB��B�B�NB��B�-B��B��B�bBy�BhsBZBQ�BN�BL�BI�B?}B/B"�B�BPB
��B
�yB
�5B
��B
ƨB
B
�XB
�B
��B
��B
��B
�hB
�=B
�+B
� B
w�B
r�B
n�B
hsB
dZB
bNB
aHB
]/B
\)B
XB
S�B
P�B
O�B
M�B
I�B
B�B
>wB
;dB
9XB
8RB
8RB
7LB
6FB
33B
1'B
/B
,B
,B
(�B
%�B
�B
�B
�B
hB
VB
JB
DB

=B
1B
B
B
B
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�yB	�mB	�ZB	�;B	�)B	�B	�B	��B	��B	��B	��B	��B	��B	��B	ƨB	ĜB	ÖB	��B	��B	�}B	�qB	�qB	�^B	�XB	�RB	�9B	�3B	�'B	�!B	�B	�'B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�oB	�bB	�\B	�PB	�=B	�+B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�+B	�1B	�=B	�JB	�PB	�VB	��B	��B	�uB	��B	�\B	y�B	q�B	r�B	r�B	r�B	q�B	r�B	r�B	r�B	s�B	s�B	t�B	s�B	s�B	s�B	s�B	u�B	u�B	x�B	y�B	z�B	{�B	{�B	|�B	|�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�DB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�-B	�9B	�?B	�RB	�XB	�XB	�^B	�wB	��B	B	ÖB	ŢB	ŢB	ŢB	ƨB	ȴB	ȴB	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�#B	�#B	�#B	�/B	�/B	�/B	�/B	�;B	�ZB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
1B

=B
DB
PB
hB
�B
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
'�B
,B
.B
/B
1'B
2-B
5?B
:^B
<jB
?}B
@�B
@�B
B�B
H�B
K�B
O�B
P�B
R�B
R�B
R�B
VB
W
B
W
B
ZB
[#B
^5B
_;B
aHB
ffB
gmB
hsB
hsB
hsB
iyB
iyB
l�B
p�B
r�B
s�B
t�B
t�B
u�B
v�B
y�B
z�B
z�B
|�B
� B
�B
�7B
�7B
�7B
�=B
�DB
�PB
�\B
�hB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�-B
�?B
�XB
�^B
�^B
�^B
�^B
�dB
�dB
�jB
�wB
��B
B
ÖB
ĜB
ŢB
ȴB
ɺB
��B
��B
��B
�B
�/B
�5B
�HB
�NB
�TB
�`B
�fB
�sB
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B  B  BBBB%B1B
=BJBJBVBbBuBuB�B�B�B�B�B�B�B �B"�B"�B"�B#�B#�B$�B%�B&�B'�B'�B'�B'�B)�B+B+B,B,B0!B1'B2-B2-B33B6FB8RB8RB9XB9XB9XB:^B<jB<jB<jB<jB<jB?}B@�BA�BC�BD�BE�BE�BE�BF�BF�BG�BG�BH�BI�BK�BM�BN�BO�BN�BO�BP�BQ�BQ�BQ�BR�BS�BS�BT�BXBXBXBYBZBZB[#B[#B\)B^5B`BBaHBaHBaHBbNBcTBcTBbNBcTBdZBe`BffBffBgmBhsBhsBjBk�Bl�Bk�Bm�Bo�Bp�Bp�Bp�Bp�Bq�Bq�Br�Bq�Bq�Br�Br�Br�Bs�Bs�Bs�Bt�Bt�Bu�Bv�Bv�Bv�Bw�Bw�Bw�Bx�Bx�By�Bz�B|�B|�B|�B~�B~�B~�B� B�B�B�B�B�B�B�B�B�B�%B�%B�%B�+B�1B�1B�7B�7B�7B�7B�7B�7B�7B�7B�=B�DB�DB�DB�JB�JB�JB�JB�PB�VB�\B�bB�bB�bB�bB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�!B�'B�'B�-B�-B�-B�3B�3B�3B�9B�?B�?B�?B�?B�FB�FB�LB�LB�RB�RB�RB�RB�RB�XB�XB�XB�XB�^B�^B�^B�^B�dB�dB�dB�jB�jB�jB�qB�qB�wB�wB�wB�}B�}B�}B�}B�}B��B��B��B��B��B��B��BBBBBÖBÖBÖBÖBĜBĜBŢBŢBŢBŢBƨBƨBǮBǮBǮBǮBǮBǮBȴBȴBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�
B�B�
B�
B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�#B�#B�#B�#B�#B�#B�#B�#B�)B�)B�)B�)B�)B�/B�/B�/B�/B�/B�/B�/B�/B�/B�5B�5B�;B�;B�;B�;B�;B�;B�BB�BB�BB�BB�HB�HB�HB�NB�NB�NB�NB�NB�NB�TB�TB�TB�TB�TB�TB�ZB�ZB�Z44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BB�B�NB��B�jB��B�oB�DB� B{�BffB\)BJ�B9XB.B�BoB+BB��B�fB��B�^B�'B�B��B��B�7B}�Bv�Bq�Bm�BYBN�B;dB%�B�BB��B�B�NB��B�-B��B��B�bBy�BhsBZBQ�BN�BL�BI�B?}B/B"�B�BPB
��B
�yB
�5B
��B
ƨB
B
�XB
�B
��B
��B
��B
�hB
�=B
�+B
� B
w�B
r�B
n�B
hsB
dZB
bNB
aHB
]/B
\)B
XB
S�B
P�B
O�B
M�B
I�B
B�B
>wB
;dB
9XB
8RB
8RB
7LB
6FB
33B
1'B
/B
,B
,B
(�B
%�B
�B
�B
�B
hB
VB
JB
DB

=B
1B
B
B
B
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�yB	�mB	�ZB	�;B	�)B	�B	�B	��B	��B	��B	��B	��B	��B	��B	ƨB	ĜB	ÖB	��B	��B	�}B	�qB	�qB	�^B	�XB	�RB	�9B	�3B	�'B	�!B	�B	�'B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�oB	�bB	�\B	�PB	�=B	�+B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�+B	�1B	�=B	�JB	�PB	�VB	��B	��B	�uB	��B	�\B	y�B	q�B	r�B	r�B	r�B	q�B	r�B	r�B	r�B	s�B	s�B	t�B	s�B	s�B	s�B	s�B	u�B	u�B	x�B	y�B	z�B	{�B	{�B	|�B	|�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�DB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�-B	�9B	�?B	�RB	�XB	�XB	�^B	�wB	��B	B	ÖB	ŢB	ŢB	ŢB	ƨB	ȴB	ȴB	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�#B	�#B	�#B	�/B	�/B	�/B	�/B	�;B	�ZB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
1B

=B
DB
PB
hB
�B
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
'�B
,B
.B
/B
1'B
2-B
5?B
:^B
<jB
?}B
@�B
@�B
B�B
H�B
K�B
O�B
P�B
R�B
R�B
R�B
VB
W
B
W
B
ZB
[#B
^5B
_;B
aHB
ffB
gmB
hsB
hsB
hsB
iyB
iyB
l�B
p�B
r�B
s�B
t�B
t�B
u�B
v�B
y�B
z�B
z�B
|�B
� B
�B
�7B
�7B
�7B
�=B
�DB
�PB
�\B
�hB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�-B
�?B
�XB
�^B
�^B
�^B
�^B
�dB
�dB
�jB
�wB
��B
B
ÖB
ĜB
ŢB
ȴB
ɺB
��B
��B
��B
�B
�/B
�5B
�HB
�NB
�TB
�`B
�fB
�sB
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B  B  BBBB%B1B
=BJBJBVBbBuBuB�B�B�B�B�B�B�B �B"�B"�B"�B#�B#�B$�B%�B&�B'�B'�B'�B'�B)�B+B+B,B,B0!B1'B2-B2-B33B6FB8RB8RB9XB9XB9XB:^B<jB<jB<jB<jB<jB?}B@�BA�BC�BD�BE�BE�BE�BF�BF�BG�BG�BH�BI�BK�BM�BN�BO�BN�BO�BP�BQ�BQ�BQ�BR�BS�BS�BT�BXBXBXBYBZBZB[#B[#B\)B^5B`BBaHBaHBaHBbNBcTBcTBbNBcTBdZBe`BffBffBgmBhsBhsBjBk�Bl�Bk�Bm�Bo�Bp�Bp�Bp�Bp�Bq�Bq�Br�Bq�Bq�Br�Br�Br�Bs�Bs�Bs�Bt�Bt�Bu�Bv�Bv�Bv�Bw�Bw�Bw�Bx�Bx�By�Bz�B|�B|�B|�B~�B~�B~�B� B�B�B�B�B�B�B�B�B�B�%B�%B�%B�+B�1B�1B�7B�7B�7B�7B�7B�7B�7B�7B�=B�DB�DB�DB�JB�JB�JB�JB�PB�VB�\B�bB�bB�bB�bB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�!B�'B�'B�-B�-B�-B�3B�3B�3B�9B�?B�?B�?B�?B�FB�FB�LB�LB�RB�RB�RB�RB�RB�XB�XB�XB�XB�^B�^B�^B�^B�dB�dB�dB�jB�jB�jB�qB�qB�wB�wB�wB�}B�}B�}B�}B�}B��B��B��B��B��B��B��BBBBBÖBÖBÖBÖBĜBĜBŢBŢBŢBŢBƨBƨBǮBǮBǮBǮBǮBǮBȴBȴBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�
B�B�
B�
B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�#B�#B�#B�#B�#B�#B�#B�#B�)B�)B�)B�)B�)B�/B�/B�/B�/B�/B�/B�/B�/B�/B�5B�5B�;B�;B�;B�;B�;B�;B�BB�BB�BB�BB�HB�HB�HB�NB�NB�NB�NB�NB�NB�TB�TB�TB�TB�TB�TB�ZB�ZB�Z44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210218102941                              AO  ARCAADJP                                                                    20210218102941    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210218102941  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210218102941  QCF$                G�O�G�O�G�O�8000            
CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:53:07Z creation;2022-06-04T17:53:07Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175307  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               0A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�w`U1   @���5@0y�"��`�c;�E���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A��A   A@  A^ffA�  A�33A�33A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BJffBNffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C �C�fC  C�fC  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF33CG�fCJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D��3D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�\)AG�A�A?�A^zA�A�
=A�
=A��
A��
A��
A��
A��
A�
=B�B�B�B�B'�B/�B7�B?�BJQ�BNQ�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B��]B��]B���B���B���B���B�B���B���B���B���B���B���B���B���B�(�B���B���B���B���C {C�GC��C�GC��C	��C��C��C��C��C��C��C��C��C��C{C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CD{CF.CG�GCI��CK��CM��CO��CQ��CS��CU�GCW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Ct{Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D>D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D�D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\D�D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�B�D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�A�A��A�#:A�)�A�.A�/�A�.�A�/OA�-�A�*�A�*�A�+�A�,A�*�A�)_A�,�A�,qA�&LA�A���A���A�H�A�|A�c�A��Aǈ1A�MA��3A�u%A��zA�ܒA� iAÄA�0�A��dA�e�A���A��uA�qA��A�~]A�:A���A��/A��rA��A��A� �A�՛A��RA���A��A���A�|�A���A��aA�� A�?HA�j�A��A�ʌA��A��A���A�<jA�+A�бA���A���A���A�OvA���A�TaA���A���A���A�J#A�F�A�A�wfA���A��FA���A�(XA���A��'A�W�A���A���A��FA��A��A��tA��0A��A�A{��AyJAu��Ar��An�FAk֡Ag�AAbw�A]��A[��AY��AX~(AV'�AT�AS@�AR��AQ �AM��AK�AH��AE�OAD�aAC]dAAU�A<�;A<�rA=/�A<�A8l�A7 \A5�xA3]dA1�A.c A-qvA-A+��A+�A+%FA*�A)YKA)��A)�fA*"�A)c�A&�A%h
A$Z�A#7A!L�A ��A!��A"OA!��A`BA^�Ak�A7A��AƨA��A��A��A�AxAk�AkQA�ArGAk�A�KA�Au�A$tA�)AJ#A)_A�_A�A�[A�AA�aA�}A�bAߤA:*A��A,=A��AAYA(�A
o A
A	f�A	eA�A�KA�AqvA9�A�A��Aj�A+A��A�4A�A�:AFAA�rAC�A�A�FAL0A/�A��A�A �HA ��A +@�(@���@�V@�{�@��9@��9@�ԕ@�f�@���@�\�@���@��w@���@���@�hs@�"�@���@�0U@�^@��@�J@���@�@��@��@�z@�*�@���@섶@���@�@@�K^@�RT@��|@�w�@�_@�o�@�
=@�R@�@�8@�N�@��@�^@��@�6z@��,@�|�@��@���@��g@���@�خ@���@� �@�Dg@��@ߡ�@߫�@���@ߔ�@ޭ�@�*�@ݲ-@܌@��@�1@��3@��@�J�@��z@تe@���@��@�1�@��'@�m�@�1@�:�@��]@�o @�@��@�S@�6�@�5�@ν<@���@��@��@�x@�q@��D@�k�@�U2@���@˜�@ˊ�@˪�@˜@��@ʧ�@ʧ�@�s�@�Z@�$@���@���@�x@�G�@���@ȋD@�d�@�5?@��@�@�Z�@�,=@��@��m@��d@ť�@�H�@��@�#:@Ì~@Ò:@�-w@¡b@���@���@��,@��_@�[�@�ԕ@�� @�B[@���@�w2@��@�K�@�[�@�U�@�ѷ@�|�@���@��u@���@��@�"h@�ϫ@�u�@�m]@���@�@�I�@���@���@��'@�-w@�U2@�"h@��6@���@�zx@�	l@�Q�@�9�@�5�@��@���@��O@�h�@���@�v`@�W?@���@���@�U2@�@�@��;@��{@�_p@�0�@���@�q�@�L0@��@��@�^�@��@��@�-@�o�@�9�@�)_@��Z@���@���@�N<@���@�@�@�(�@���@��@��H@���@���@�$�@���@��Q@�v`@�7L@�S@��5@��@��O@�s�@�[�@��o@�6z@���@��o@�v�@�L0@�7@�ԕ@�2a@��@���@���@�q�@�C-@�$�@�6�@��#@��E@��@���@�|�@�a�@��@�H�@��@���@�C-@��@��W@��@���@��7@��@�Ɇ@�Xy@��'@�s@�`B@�@O@��@�($@� �@��o@���@��@�y�@�N<@�{�@�ff@�I�@��@��C@�]�@�(�@��@��@��@��A@��@��k@���@���@��@��@��"@�_p@�	l@���@���@�Ft@��@��D@��z@�`B@��M@��I@���@�}V@�U2@�!@��N@�N<@��"@���@��@�p;@�[�@�  @���@��V@���@�x@�RT@�'�@�%@��@��@�u�@�?@�u@��@���@���@��@�|@�dZ@�W?@�:�@��X@��\@�_�@�6�@��@���@��@�n/@�N<@��@�@��@���@�xl@�-�@��T@�j@��@���@��9@�M@���@���@�@���@�c�@�9X@�@���@��Q@��N@���@�qv@�+�@��@���@�q�@�7�@��.@��>@��}@���@�g�@�4@��@��,@���@���@�q�@�_@E9@~��@~}V@~GE@}�@}��@}�@}�@}�M@}c@}x�@}x�@}Vm@}5�@|��@|�9@|�@|]d@|S�@|7�@{�A@{�@{�m@{˒@zȴ@zB[@y�j@y��@yf�@y0�@x��@x��@w�g@w(@v�@vl�@v@uV@t��@t~@s�g@s��@s��@sU�@r�s@r��@r��@r�r@r�@q�^@q5�@p�j@pj@p1@o�[@o��@o|�@oS�@o$t@n�@nv�@n�@m�H@m�7@mu�@mp�@mG�@lZ@k��@kiD@k�@j��@jc @j#:@i@h�5@h[�@g�
@gE9@f��@fE�@e��@e2a@dj@d7@cƨ@c��@c��@cX�@b�@bZ�@b�@a��@a��@a��@aIR@`tT@_�@_��@_!-@^ں@^��@]�Z@]zx@]�@\��@\b@[�@[e�@Z��@Z\�@Y�@Yc@YQ�@Y(�@X�@X�@WU�@W$t@V�@V��@VJ�@V;�@V�@U��@U�H@UVm@T��@T�[@T��@T�o@S�w@R�H@Rȴ@R�<@R��@Q�@Q^�@P�9@PM@O�}@Oe�@O;d@N�]@M��@M��@Mc�@M@M�@LɆ@LQ�@L�@K�@K�@K�&@K�Q@K��@KJ#@J�R@Jh
@J@�@J.�@J�@I�Z@I��@I�^@I��@I5�@I�@H�D@Hg8@G~�@G"�@G�@F�@F4@E�@E��@E:�@D�@DĜ@D�9@D�@Dw�@D1@C��@C~�@CS�@B��@Bff@B@Aϫ@A��@AS&@@��@@b@?��@?S�@?8@>��@>e@=��@=�N@=ԕ@=�^@=c@<�e@<�@;��@;�@;x@;8@:�@:��@:u%@:�@9�@9��@9�-@97L@8r�@8M@8(�@8�@7�@7��@7b�@7�@6�6@68�@5�@5��@4�@4��@4:�@3� @3��@3~�@3K�@3 i@2ߤ@2�<@2��@2ff@2R�@2_@1�C@1u�@0�|@0Ĝ@0A�@/�}@/��@/U�@/33@/�@.ں@.��@.s�@.W�@.	@-�>@-�7@,�@,�@,2�@,�@+��@+��@+��@+e�@+Y@*��@*d�@*L0@*E�@*6�@*	@)�z@(�@(��@(H@'�@'˒@'��@'y�@'dZ@'33@&ߤ@&3�@%��@%G�@$�@$��@$�I@$�_@$<�@$�@#�:@#(@"��@"��@"�A@"+k@!�@!��@!�=@!��@!+@ �)@ �@ c�@ M@ 6@�}@��@S�@Y@�@YK@��@��@�S@rG@+@��@c�@D�@/�@!@�+@��@�f@x@n/@]�@9�@�@��@xl@)�@ �@�z@5�@��@֡@�@M@!@��@n/@�@��@��@�@a|@!�@�@�Z@c@F@�@�@�@��@�@��@g�@�@��@ں@҉@��@�L@�F@�+@}V@p;@d�@GE@$�@	@@zx@�?@��@w�@tT@r�@h�@N�@9X@�@�@��@��@|�@@O@33@.I@$t@�@ i@�@�s@��@�@�F@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�A�A��A�#:A�)�A�.A�/�A�.�A�/OA�-�A�*�A�*�A�+�A�,A�*�A�)_A�,�A�,qA�&LA�A���A���A�H�A�|A�c�A��Aǈ1A�MA��3A�u%A��zA�ܒA� iAÄA�0�A��dA�e�A���A��uA�qA��A�~]A�:A���A��/A��rA��A��A� �A�՛A��RA���A��A���A�|�A���A��aA�� A�?HA�j�A��A�ʌA��A��A���A�<jA�+A�бA���A���A���A�OvA���A�TaA���A���A���A�J#A�F�A�A�wfA���A��FA���A�(XA���A��'A�W�A���A���A��FA��A��A��tA��0A��A�A{��AyJAu��Ar��An�FAk֡Ag�AAbw�A]��A[��AY��AX~(AV'�AT�AS@�AR��AQ �AM��AK�AH��AE�OAD�aAC]dAAU�A<�;A<�rA=/�A<�A8l�A7 \A5�xA3]dA1�A.c A-qvA-A+��A+�A+%FA*�A)YKA)��A)�fA*"�A)c�A&�A%h
A$Z�A#7A!L�A ��A!��A"OA!��A`BA^�Ak�A7A��AƨA��A��A��A�AxAk�AkQA�ArGAk�A�KA�Au�A$tA�)AJ#A)_A�_A�A�[A�AA�aA�}A�bAߤA:*A��A,=A��AAYA(�A
o A
A	f�A	eA�A�KA�AqvA9�A�A��Aj�A+A��A�4A�A�:AFAA�rAC�A�A�FAL0A/�A��A�A �HA ��A +@�(@���@�V@�{�@��9@��9@�ԕ@�f�@���@�\�@���@��w@���@���@�hs@�"�@���@�0U@�^@��@�J@���@�@��@��@�z@�*�@���@섶@���@�@@�K^@�RT@��|@�w�@�_@�o�@�
=@�R@�@�8@�N�@��@�^@��@�6z@��,@�|�@��@���@��g@���@�خ@���@� �@�Dg@��@ߡ�@߫�@���@ߔ�@ޭ�@�*�@ݲ-@܌@��@�1@��3@��@�J�@��z@تe@���@��@�1�@��'@�m�@�1@�:�@��]@�o @�@��@�S@�6�@�5�@ν<@���@��@��@�x@�q@��D@�k�@�U2@���@˜�@ˊ�@˪�@˜@��@ʧ�@ʧ�@�s�@�Z@�$@���@���@�x@�G�@���@ȋD@�d�@�5?@��@�@�Z�@�,=@��@��m@��d@ť�@�H�@��@�#:@Ì~@Ò:@�-w@¡b@���@���@��,@��_@�[�@�ԕ@�� @�B[@���@�w2@��@�K�@�[�@�U�@�ѷ@�|�@���@��u@���@��@�"h@�ϫ@�u�@�m]@���@�@�I�@���@���@��'@�-w@�U2@�"h@��6@���@�zx@�	l@�Q�@�9�@�5�@��@���@��O@�h�@���@�v`@�W?@���@���@�U2@�@�@��;@��{@�_p@�0�@���@�q�@�L0@��@��@�^�@��@��@�-@�o�@�9�@�)_@��Z@���@���@�N<@���@�@�@�(�@���@��@��H@���@���@�$�@���@��Q@�v`@�7L@�S@��5@��@��O@�s�@�[�@��o@�6z@���@��o@�v�@�L0@�7@�ԕ@�2a@��@���@���@�q�@�C-@�$�@�6�@��#@��E@��@���@�|�@�a�@��@�H�@��@���@�C-@��@��W@��@���@��7@��@�Ɇ@�Xy@��'@�s@�`B@�@O@��@�($@� �@��o@���@��@�y�@�N<@�{�@�ff@�I�@��@��C@�]�@�(�@��@��@��@��A@��@��k@���@���@��@��@��"@�_p@�	l@���@���@�Ft@��@��D@��z@�`B@��M@��I@���@�}V@�U2@�!@��N@�N<@��"@���@��@�p;@�[�@�  @���@��V@���@�x@�RT@�'�@�%@��@��@�u�@�?@�u@��@���@���@��@�|@�dZ@�W?@�:�@��X@��\@�_�@�6�@��@���@��@�n/@�N<@��@�@��@���@�xl@�-�@��T@�j@��@���@��9@�M@���@���@�@���@�c�@�9X@�@���@��Q@��N@���@�qv@�+�@��@���@�q�@�7�@��.@��>@��}@���@�g�@�4@��@��,@���@���@�q�@�_@E9@~��@~}V@~GE@}�@}��@}�@}�@}�M@}c@}x�@}x�@}Vm@}5�@|��@|�9@|�@|]d@|S�@|7�@{�A@{�@{�m@{˒@zȴ@zB[@y�j@y��@yf�@y0�@x��@x��@w�g@w(@v�@vl�@v@uV@t��@t~@s�g@s��@s��@sU�@r�s@r��@r��@r�r@r�@q�^@q5�@p�j@pj@p1@o�[@o��@o|�@oS�@o$t@n�@nv�@n�@m�H@m�7@mu�@mp�@mG�@lZ@k��@kiD@k�@j��@jc @j#:@i@h�5@h[�@g�
@gE9@f��@fE�@e��@e2a@dj@d7@cƨ@c��@c��@cX�@b�@bZ�@b�@a��@a��@a��@aIR@`tT@_�@_��@_!-@^ں@^��@]�Z@]zx@]�@\��@\b@[�@[e�@Z��@Z\�@Y�@Yc@YQ�@Y(�@X�@X�@WU�@W$t@V�@V��@VJ�@V;�@V�@U��@U�H@UVm@T��@T�[@T��@T�o@S�w@R�H@Rȴ@R�<@R��@Q�@Q^�@P�9@PM@O�}@Oe�@O;d@N�]@M��@M��@Mc�@M@M�@LɆ@LQ�@L�@K�@K�@K�&@K�Q@K��@KJ#@J�R@Jh
@J@�@J.�@J�@I�Z@I��@I�^@I��@I5�@I�@H�D@Hg8@G~�@G"�@G�@F�@F4@E�@E��@E:�@D�@DĜ@D�9@D�@Dw�@D1@C��@C~�@CS�@B��@Bff@B@Aϫ@A��@AS&@@��@@b@?��@?S�@?8@>��@>e@=��@=�N@=ԕ@=�^@=c@<�e@<�@;��@;�@;x@;8@:�@:��@:u%@:�@9�@9��@9�-@97L@8r�@8M@8(�@8�@7�@7��@7b�@7�@6�6@68�@5�@5��@4�@4��@4:�@3� @3��@3~�@3K�@3 i@2ߤ@2�<@2��@2ff@2R�@2_@1�C@1u�@0�|@0Ĝ@0A�@/�}@/��@/U�@/33@/�@.ں@.��@.s�@.W�@.	@-�>@-�7@,�@,�@,2�@,�@+��@+��@+��@+e�@+Y@*��@*d�@*L0@*E�@*6�@*	@)�z@(�@(��@(H@'�@'˒@'��@'y�@'dZ@'33@&ߤ@&3�@%��@%G�@$�@$��@$�I@$�_@$<�@$�@#�:@#(@"��@"��@"�A@"+k@!�@!��@!�=@!��@!+@ �)@ �@ c�@ M@ 6@�}@��@S�@Y@�@YK@��@��@�S@rG@+@��@c�@D�@/�@!@�+@��@�f@x@n/@]�@9�@�@��@xl@)�@ �@�z@5�@��@֡@�@M@!@��@n/@�@��@��@�@a|@!�@�@�Z@c@F@�@�@�@��@�@��@g�@�@��@ں@҉@��@�L@�F@�+@}V@p;@d�@GE@$�@	@@zx@�?@��@w�@tT@r�@h�@N�@9X@�@�@��@��@|�@@O@33@.I@$t@�@ i@�@�s@��@�@�F@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	+B	+6B	+6B	+6B	+6B	*�B	*�B	*�B	*�B	*KB	*�B	*B	*B	*KB	*�B	*B	*B	*eB	*eB	*B	*KB	)�B	)_B	 �B	"NB	b�B	��B	�,B	��B	��B	��B	�tB	��B
YB
YB
q�B
{�B
�iB
�?B
�fB
��B
��B
�BB
�SB
��B
�B
�B
y�B
_!B
[qB
wB
��B
�sB
�uB�ByB�B7LBE�BH1Ba�B�+B�tB��B�B�lBuB�fBUB�BgB�B�B�B�B�B��B��B�B�)B�EB�$B�B�oBg�B1�B
��B
�pB
�pB�B
�B"B
�;B
�VB
tB
2�B	�B	ŢB	��B	��B	v�B	]/B	AB	3MB	#�B	HB	"B	zB	uB	 �B�VB	 B	�B	
	B	�B	�B	�B	&fB	CB	<�B	4�B	*0B	
�B	�B	5�B	5�B	!�B	�B	�B��B��B�hB�-B�LB�"B�B��B��B	�B	'B	+�B	3hB	4B	'�B	�B	B	/�B	0�B	,�B	BuB	X+B	\]B	I�B	I�B	aHB	i_B	s�B	xlB	a-B	a�B	i*B	z�B	�AB	��B	�BB	��B	��B	�wB	�B	��B	�B	��B	��B	�&B	�B	�bB	�B	�vB	�GB	�aB	�+B	�B	�B	�PB	�^B	��B	��B	�tB	�hB	��B	��B	�!B	��B	��B	��B	�nB	�8B	�B	�^B	�*B	��B	�<B	�(B	��B	��B	��B	��B	�.B	��B	�BB	�wB	��B	�PB	�B	��B	�VB	��B	�JB	�B	�DB	��B	�>B	�B	�fB	��B	�`B	�lB	��B	�B	�iB	� B	�B	�aB	�{B	��B	��B	��B	ðB	ðB	��B	�B	ŢB	ňB	ƨB	żB	żB	�mB	�%B	�B	ƨB	��B	ǔB	�B	ǮB	�B	ȴB	�B	ɠB	��B	�6B	��B	�vB	� B	� B	� B	�4B	�hB	� B	ҽB	��B	�aB	�MB	ևB	��B	�)B	�)B	ۦB	ںB	�jB	�B	�`B	�ZB	�nB	�,B	�zB	�B	��B	��B	�2B	�fB	�B	�tB	�B	�B	�B	�hB	��B	�B	��B	ߊB	�CB	��B	��B	ݲB	یB	�+B	ևB	�FB	�B	�&B	��B	֡B	�ZB	�B	�B	��B	�B	��B	�tB	�B	�B	��B	�B	�B	�fB	�8B	�B	��B	��B	�B	�B	�QB	��B	�B	�=B	�WB	�QB	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�KB	�_B	�B	��B	��B	�5B	�B	�B	�kB	��B	�B	��B	��B	�B	� B	��B	��B	�B	�B	��B	��B	�B	�;B	�OB	�B	��B	�TB	��B	�B	��B	�hB	��B	�hB	�B	�OB	�5B	�5B	�iB	�B	�TB	�+B	��B	��B	��B	��B	��B	�B	��B	��B	�jB	�PB	�6B	�B	�jB	�jB	�6B	�PB	��B	��B	�jB	�jB	�jB	��B	�VB	��B	�VB	��B	��B	��B	�B	�6B	�jB	��B	��B	�6B	�jB	�6B	��B	�B	�dB	��B	��B	�B	�.B	��B
oB
B
AB
�B
�B
�B
�B
GB
B
�B
�B
�B
B
�B
B
B
�B
�B
B
�B
B
+B
�B
	7B
�B
�B
�B
	�B
dB
B
B
�B
VB
�B
B
�B
�B
�B
�B
vB
vB
�B
B
�B
�B
vB
�B
\B
vB
�B
�B
�B
�B
�B
hB
B
TB
�B
 B
�B
TB
uB
&B
[B
�B
�B
,B
B
�B
B
mB
�B
�B
$B
�B
?B

B
$B
+B
_B
�B
�B
�B
�B
�B
�B
�B
eB
7B
�B
#B
=B
�B
�B
)B
�B
�B
�B
/B
~B
�B
�B
B
B
�B
�B
!B
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
!HB
!|B
!|B
!�B
"�B
"NB
"hB
"�B
"�B
"�B
#nB
$ZB
$�B
%,B
%�B
%�B
%�B
&�B
'mB
'�B
'�B
($B
(�B
(�B
(�B
)DB
)*B
)B
)_B
)DB
)�B
*0B
*KB
*�B
*�B
+QB
+QB
+QB
+kB
+�B
,=B
,WB
,�B
,�B
,�B
,�B
-)B
-wB
-wB
-�B
.}B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/B
/OB
/iB
/iB
/OB
/iB
/�B
/�B
/�B
/OB
0oB
0�B
1[B
1�B
2B
2GB
2|B
2aB
3MB
3�B
3�B
4B
4�B
5?B
5�B
5�B
5�B
6�B
6�B
6�B
7LB
72B
72B
7B
7fB
7fB
7�B
7�B
8B
8RB
8lB
8�B
8�B
8�B
8�B
8�B
9$B
9XB
9�B
9�B
9�B
9�B
9�B
:xB
:�B
:�B
;JB
;B
;�B
;�B
<PB
="B
=�B
>B
>BB
>�B
>�B
?HB
?�B
@ B
@OB
@iB
@�B
@�B
@�B
@�B
AoB
AoB
A�B
A�B
A�B
A�B
B[B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
DB
DMB
D�B
D�B
D�B
EB
E�B
E�B
F%B
FYB
FtB
F�B
GEB
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H1B
HB
HfB
H�B
H�B
H�B
H�B
IlB
J	B
I�B
I�B
J=B
J�B
K)B
K�B
K�B
LB
LB
LB
L0B
L�B
L�B
MB
MB
MB
MPB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N"B
N�B
N�B
N�B
N�B
OB
OB
N�B
O(B
OBB
OvB
O\B
O�B
O�B
P�B
P�B
P�B
P�B
QNB
Q4B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q4B
QhB
QhB
Q4B
Q4B
Q�B
Q�B
Q�B
RoB
R�B
SB
S&B
SB
R�B
SuB
TB
TB
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
UB
UMB
UMB
UgB
U�B
U�B
VB
U�B
V9B
V�B
V�B
V�B
V�B
W
B
W?B
WYB
W�B
W�B
XEB
XyB
X�B
YKB
YB
Y�B
ZB
ZB
ZQB
Z�B
[	B
[#B
[#B
[WB
[�B
[�B
\B
\]B
\�B
]/B
]IB
^B
^jB
^�B
^�B
^�B
_B
_!B
_�B
_�B
_�B
_�B
_�B
`B
`�B
`�B
`�B
a-B
a-B
abB
abB
a�B
bB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
d&B
d@B
d@B
d@B
d@B
dtB
eB
eFB
fB
f�B
f�B
f�B
f�B
f�B
f�B
g�B
h�B
iB
i*B
iB
i�B
jB
j0B
jB
jB
jKB
jB
jB
j�B
j�B
j�B
kB
k6B
kQB
kkB
kkB
lB
l=B
lqB
l�B
l�B
l�B
m)B
m]B
mwB
mwB
m�B
m�B
m�B
n/B
nB
n/B
n/B
n/B
n�B
n�B
n�B
oOB
o5B
oiB
o�B
pB
o�B
p;B
pUB
poB
p�B
p�B
qAB
q[B
q�B
q�B
rB
r-B
rB
rGB
r�B
r�B
s3B
s3B
sMB
s3B
s�B
tB
t�B
t�B
u%B
u%B
u%B
u?B
uZB
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
xRB
xlB
x�B
x�B
x�B
y$B
yXB
yrB
yXB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	+B	+6B	+6B	+6B	+6B	*�B	*�B	*�B	*�B	*KB	*�B	*B	*B	*KB	*�B	*B	*B	*eB	*eB	*B	*KB	)�B	)_B	 �B	"NB	b�B	��B	�,B	��B	��B	��B	�tB	��B
YB
YB
q�B
{�B
�iB
�?B
�fB
��B
��B
�BB
�SB
��B
�B
�B
y�B
_!B
[qB
wB
��B
�sB
�uB�ByB�B7LBE�BH1Ba�B�+B�tB��B�B�lBuB�fBUB�BgB�B�B�B�B�B��B��B�B�)B�EB�$B�B�oBg�B1�B
��B
�pB
�pB�B
�B"B
�;B
�VB
tB
2�B	�B	ŢB	��B	��B	v�B	]/B	AB	3MB	#�B	HB	"B	zB	uB	 �B�VB	 B	�B	
	B	�B	�B	�B	&fB	CB	<�B	4�B	*0B	
�B	�B	5�B	5�B	!�B	�B	�B��B��B�hB�-B�LB�"B�B��B��B	�B	'B	+�B	3hB	4B	'�B	�B	B	/�B	0�B	,�B	BuB	X+B	\]B	I�B	I�B	aHB	i_B	s�B	xlB	a-B	a�B	i*B	z�B	�AB	��B	�BB	��B	��B	�wB	�B	��B	�B	��B	��B	�&B	�B	�bB	�B	�vB	�GB	�aB	�+B	�B	�B	�PB	�^B	��B	��B	�tB	�hB	��B	��B	�!B	��B	��B	��B	�nB	�8B	�B	�^B	�*B	��B	�<B	�(B	��B	��B	��B	��B	�.B	��B	�BB	�wB	��B	�PB	�B	��B	�VB	��B	�JB	�B	�DB	��B	�>B	�B	�fB	��B	�`B	�lB	��B	�B	�iB	� B	�B	�aB	�{B	��B	��B	��B	ðB	ðB	��B	�B	ŢB	ňB	ƨB	żB	żB	�mB	�%B	�B	ƨB	��B	ǔB	�B	ǮB	�B	ȴB	�B	ɠB	��B	�6B	��B	�vB	� B	� B	� B	�4B	�hB	� B	ҽB	��B	�aB	�MB	ևB	��B	�)B	�)B	ۦB	ںB	�jB	�B	�`B	�ZB	�nB	�,B	�zB	�B	��B	��B	�2B	�fB	�B	�tB	�B	�B	�B	�hB	��B	�B	��B	ߊB	�CB	��B	��B	ݲB	یB	�+B	ևB	�FB	�B	�&B	��B	֡B	�ZB	�B	�B	��B	�B	��B	�tB	�B	�B	��B	�B	�B	�fB	�8B	�B	��B	��B	�B	�B	�QB	��B	�B	�=B	�WB	�QB	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�KB	�_B	�B	��B	��B	�5B	�B	�B	�kB	��B	�B	��B	��B	�B	� B	��B	��B	�B	�B	��B	��B	�B	�;B	�OB	�B	��B	�TB	��B	�B	��B	�hB	��B	�hB	�B	�OB	�5B	�5B	�iB	�B	�TB	�+B	��B	��B	��B	��B	��B	�B	��B	��B	�jB	�PB	�6B	�B	�jB	�jB	�6B	�PB	��B	��B	�jB	�jB	�jB	��B	�VB	��B	�VB	��B	��B	��B	�B	�6B	�jB	��B	��B	�6B	�jB	�6B	��B	�B	�dB	��B	��B	�B	�.B	��B
oB
B
AB
�B
�B
�B
�B
GB
B
�B
�B
�B
B
�B
B
B
�B
�B
B
�B
B
+B
�B
	7B
�B
�B
�B
	�B
dB
B
B
�B
VB
�B
B
�B
�B
�B
�B
vB
vB
�B
B
�B
�B
vB
�B
\B
vB
�B
�B
�B
�B
�B
hB
B
TB
�B
 B
�B
TB
uB
&B
[B
�B
�B
,B
B
�B
B
mB
�B
�B
$B
�B
?B

B
$B
+B
_B
�B
�B
�B
�B
�B
�B
�B
eB
7B
�B
#B
=B
�B
�B
)B
�B
�B
�B
/B
~B
�B
�B
B
B
�B
�B
!B
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
!HB
!|B
!|B
!�B
"�B
"NB
"hB
"�B
"�B
"�B
#nB
$ZB
$�B
%,B
%�B
%�B
%�B
&�B
'mB
'�B
'�B
($B
(�B
(�B
(�B
)DB
)*B
)B
)_B
)DB
)�B
*0B
*KB
*�B
*�B
+QB
+QB
+QB
+kB
+�B
,=B
,WB
,�B
,�B
,�B
,�B
-)B
-wB
-wB
-�B
.}B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/B
/OB
/iB
/iB
/OB
/iB
/�B
/�B
/�B
/OB
0oB
0�B
1[B
1�B
2B
2GB
2|B
2aB
3MB
3�B
3�B
4B
4�B
5?B
5�B
5�B
5�B
6�B
6�B
6�B
7LB
72B
72B
7B
7fB
7fB
7�B
7�B
8B
8RB
8lB
8�B
8�B
8�B
8�B
8�B
9$B
9XB
9�B
9�B
9�B
9�B
9�B
:xB
:�B
:�B
;JB
;B
;�B
;�B
<PB
="B
=�B
>B
>BB
>�B
>�B
?HB
?�B
@ B
@OB
@iB
@�B
@�B
@�B
@�B
AoB
AoB
A�B
A�B
A�B
A�B
B[B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
DB
DMB
D�B
D�B
D�B
EB
E�B
E�B
F%B
FYB
FtB
F�B
GEB
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H1B
HB
HfB
H�B
H�B
H�B
H�B
IlB
J	B
I�B
I�B
J=B
J�B
K)B
K�B
K�B
LB
LB
LB
L0B
L�B
L�B
MB
MB
MB
MPB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N"B
N�B
N�B
N�B
N�B
OB
OB
N�B
O(B
OBB
OvB
O\B
O�B
O�B
P�B
P�B
P�B
P�B
QNB
Q4B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q4B
QhB
QhB
Q4B
Q4B
Q�B
Q�B
Q�B
RoB
R�B
SB
S&B
SB
R�B
SuB
TB
TB
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
UB
UMB
UMB
UgB
U�B
U�B
VB
U�B
V9B
V�B
V�B
V�B
V�B
W
B
W?B
WYB
W�B
W�B
XEB
XyB
X�B
YKB
YB
Y�B
ZB
ZB
ZQB
Z�B
[	B
[#B
[#B
[WB
[�B
[�B
\B
\]B
\�B
]/B
]IB
^B
^jB
^�B
^�B
^�B
_B
_!B
_�B
_�B
_�B
_�B
_�B
`B
`�B
`�B
`�B
a-B
a-B
abB
abB
a�B
bB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
d&B
d@B
d@B
d@B
d@B
dtB
eB
eFB
fB
f�B
f�B
f�B
f�B
f�B
f�B
g�B
h�B
iB
i*B
iB
i�B
jB
j0B
jB
jB
jKB
jB
jB
j�B
j�B
j�B
kB
k6B
kQB
kkB
kkB
lB
l=B
lqB
l�B
l�B
l�B
m)B
m]B
mwB
mwB
m�B
m�B
m�B
n/B
nB
n/B
n/B
n/B
n�B
n�B
n�B
oOB
o5B
oiB
o�B
pB
o�B
p;B
pUB
poB
p�B
p�B
qAB
q[B
q�B
q�B
rB
r-B
rB
rGB
r�B
r�B
s3B
s3B
sMB
s3B
s�B
tB
t�B
t�B
u%B
u%B
u%B
u?B
uZB
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
xRB
xlB
x�B
x�B
x�B
y$B
yXB
yrB
yXB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104954  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175307  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175307  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175307                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025314  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025314  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                
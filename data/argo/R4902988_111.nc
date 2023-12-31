CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-10-25T03:42:36Z creation;2022-10-25T03:42:37Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20221025034236  20221025040206  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               oA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @����N��1   @���ޠ#@:���n��c��9Xb1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B���B�  B�33B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  C �C  C  C�C  C
  C  C  C  C  C  C�fC�fC  C  C�C �C"�C$�C&  C(  C*�C,  C.  C/�fC2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C��C��C��C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C��C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C��C�  C��3C�  C��D   D � D  D� DfD�fD  D� D  D� D��D� D  D� D  D� D  D� D	  D	�fD
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D�fD  D� D  D�fD  Dy�D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX�fDYfDY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`fD`�fDafDa� Db  Dby�Db��Dc� Dd  Dd� De  De�fDf  Dfy�Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Duy�Du��Dv� DwfDw� Dx  Dx� DyfDy� Dz  Dz� D{  D{� D{��D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D���D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D��3D��3D�3D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�C3D�� D�� D���D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D��3D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�C3D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�<�D�|�D���D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D3D�� D�  D�@ DÃ3D�� D�  D�@ DĀ D�� D�  D�<�Dŀ D��3D�  D�@ Dƃ3D��3D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D��3D�3D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр DѼ�D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�3D�@ DԀ D�� D�3D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D܀ D�� D���D�@ D݀ D�� D�  D�@ Dހ D�� D���D�<�D߀ D�� D�  D�@ D�� D�� D�3D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�C3D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D���D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�3D�@ D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�\)@�\)A�A?�A_�A�A��
A��
A���A��
A��
A��
A��
A��
B�B�B�B�B(Q�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B�(�B���B�B���B�(�B���B���B�(�B���B�B�B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B�B���B���C {C��C��C{C��C	��C��C��C��C��C��C�GC�GC��C��C{C {C"{C${C%��C'��C*{C+��C-��C/�GC1��C4{C5��C7��C9��C;��C=��C?��CA��CC��CE�GCG��CI��CK��CM��CO��CR{CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��C��qC��qC�
>C��qC��qC��qC��qC��qC��C��qC��qC��qC�
>C��qC��qC��qC��qC�
>C��qC��C��C��qC��qC�
>C�
>C�
>C��qC��C��qC��qC��qC��qC��C��C��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC�
>C��qC�
>C��qC�
>C��qC��qC��qC�
>C��qC��C��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC�
>C�
>C��qC��C��qC�
>C��qD ~�D ��D~�DD�D��D~�D��D~�D�RD~�D��D~�D��D~�D��D~�D��D	�D
D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��DxRD��D�D��D~�D��D�D��DxRD��D~�D��D~�D��D~�D��D~�D��DxRD��D~�D��D~�D��D ~�D ��D!~�D!��D"xRD"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8�RD9~�D9��D:~�D:��D;~�D;��D<�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA�RDB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS�RDT~�DT��DU~�DU��DV~�DV��DW~�DW��DX�DYDY�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D`D`�DaDa~�Da��DbxRDb�RDc~�Dc��Dd~�Dd��De�De��DfxRDf��Dg~�Dg�RDh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��DuxRDu�RDv~�DwDw~�Dw��Dx~�DyDy~�Dy��Dz~�Dz��D{~�D{�RD|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D�D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D���D�D��D�?\D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��)D�<)D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��)D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�|)D��)D��\D�?\D���D�D��D�?\D�\D��\D��\D�?\D���D�D��\D�?\D�\D��\D��\D�B�D�\D��\D��)D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�<)D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D�D��\D�?\D�\D��\D��)D�<)D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D�D��\D�B�D�\D��)D��)D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�|)D��\D��\D�?\D�|)D��)D��\D�?\D�\D��\D��\D�<)D�|)D��)D��\D�B�D���D��\D��\D�?\D�\D��\D��\D�?\D���D�D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�D¿\D��\D�?\DÂ�Dÿ\D��\D�?\D�\DĿ\D��\D�<)D�\D�D��\D�?\DƂ�D�D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\D�D��D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\DѼ)D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��D�?\D�\DԿ\D��D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��)D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�<)D�\Dܿ\D��)D�?\D�\Dݿ\D��\D�?\D�\D޿\D��)D�<)D�\D߿\D��\D�?\D�\D�\D��D�?\D�\D�)D��)D�?\D�\D�\D��\D�?\D�\D�\D��\D�<)D�|)D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�B�D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�|)D�\D��\D�B�D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��)D�<)D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��D�?\D�\D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AҿA�y�A�q�A�:�A�2-A�,=A�&LA�"4A��A�=A�1A��A�uA�4A�\A��A�
rA�
�A�	lA�fA�1A��A�+A��A���A���A��A��OAѹXAѱ�AѫkAѥ�Aї�A�K)A�QNAĥ�A���A�� A�u�A��@A�c�A�v�A�1A�>�A��HA�|�A���A�kQA���A�7LA��tA��?A�	�A��A��xA��RA��A��A���A�OA�{�A��=A�!bA��nA�SA���A���A�J#A��kA�FA��mA�w�A�=<A��A��A��	A�tTA��YA��A�t�A�DgA��BA��A��yA���A�~�A�,=A�m)A��lA�A�уA��A�?�A�^jA��HA�_A���A��gA���A��iA�3hA���A�f2A}��A|qA{��Az�Ax�)Av�AuB[Ar��ApzxAo��An�Al \AjW�Ah&Af�9Af+Ae��Ad�FAc'�A_˒A\�[AZ�AYK�AWOvAVl�AU�ATU2AS��AR�ARiDAQ�AQ\�AQ�AP"hAO�qAM�ALAK��AK(�AJ��AIAH
�AE�AE?ADzAC`�AB
�A@�pA?�[A>�A=��A< �A;5?A:��A:��A9��A7_A6�;A6-wA52aA4?}A3�MA3�eA32�A2�A1�A1u%A0��A/�jA/\�A/�A.ںA.��A.%A-s�A,/�A*YKA(��A(YA'VA&�'A&hsA&�A$�A#p;A"�pA!��A �6A _pA 	A�A�NAS&AJA� A�kAGA3�A�A�PA`BA%FA�#A��A \A}�A��AG�A�CAcA!�AԕACA��A�SAN<AA~�AA�AaAѷA
�A
�A	�$A	'�A��AC�A�3AxA�A�AY�A(A�A��AhsA�A��A��A _@��:@���@���@���@�V�@�r@���@�J�@�@�V@�}@�`B@��@�y>@�1�@貖@�c @�YK@�J#@��@�M@��@��@��.@୬@�	@߳�@�J#@���@ޥz@�%�@�q@��@�5�@�s�@�q�@�<�@��)@я�@Хz@ψf@�:�@͊	@���@��@��@ʩ�@ʣ�@ʑ�@�^5@��@Ƀ{@�!�@ȧ@�;�@ǽ�@�|�@�!�@�ی@�~(@�e@�y�@�w�@1@���@�0�@��@��c@���@�P�@��[@��O@��@��@��D@���@��@�O@���@�|�@�8�@���@�2�@��6@���@�`B@��@��8@��z@�v�@�c�@�]d@�L0@��@��N@�+k@�hs@�ߤ@��Y@�L�@�D�@��@���@��Z@��z@���@���@���@�e�@�6@���@��@��Q@��$@��F@��@�P�@��p@��@�M@��@��"@�V�@���@��@���@��`@���@��1@�}V@�l�@��@�f�@�Mj@�q@��9@�� @�PH@��6@�@�b@���@��^@���@�}�@�`B@�O�@�B�@�:�@�6z@�6z@�-w@��@�@@��@��5@��'@�-@��.@�W?@��,@��[@�I�@���@�Y�@��@��x@�u%@�;�@���@���@��@�4@���@� i@���@���@���@�y�@�f�@�T�@�
=@��@�-�@��@��N@��z@��@�m]@�U�@�8@�8@�#�@��@��5@��b@�($@��q@�K�@�"�@� i@��P@��2@��p@�-�@���@�a|@�($@�:�@�oi@�Xy@�I�@�1@���@���@�~�@�9�@��h@���@�U2@�	@~��@}Y�@}+�@|�@|��@|u�@|N�@|9X@|7�@|�@|1@{�m@{X�@{"�@{�@z�@z��@z�b@zu@y=�@x�@x�p@x��@xM@x*�@x�@v�s@v\�@v8�@u��@u�-@u�M@t�E@se�@r�h@r#:@q��@q�D@q��@p�@p��@pz�@o�W@oA�@n�y@n��@nV@n	@m�@ne@n^5@n3�@n@m��@l֡@l~(@lz�@lj@l~@lx@k��@k��@ky�@kY@j�@j�'@j��@i�@h�4@h��@h�4@h�z@h�@hg8@h%�@f��@f��@f�@f�8@f�@f��@f��@fR�@e�Z@e�h@eX@d��@d7�@c˒@c�@c�@a�@a8�@a \@aV@a�@`�$@`S�@`A�@`�@_��@_��@_��@_>�@^�1@^��@^�A@^d�@^:*@]�o@]�@]s�@\��@[�6@[C@Z�X@Z:*@Yϫ@Y��@Y��@Y��@Ym]@YL�@Y2a@Y;@X�@Xی@XɆ@X��@X�@X��@X��@Xr�@Xe�@XH@W|�@W@O@V�c@V�'@V�@T�O@Tz�@TI�@T9X@T%�@T1@S�@@S�4@SdZ@S)_@R��@R�<@R��@R�@Rh
@Q�j@Q@Q�M@Q�M@Q/@P:�@O��@O�@N�A@N\�@N?@N)�@N�@N4@M�@M��@M4@L�5@Lm�@K��@K��@K\)@J��@J�H@J��@JYK@J)�@J�@I�@I�Z@I��@I�@I}�@IIR@I=�@I7L@Hی@Hq@H[�@G�:@G i@F�<@Fh
@E�T@Em]@E@D�[@D��@DtT@D"h@C��@B�m@BV@A��@A��@A��@Ac@Ak�@A[W@A8�@@ѷ@@m�@?��@?=@?�@?�@>�H@>�6@>L0@>O@=�Z@=��@<��@<oi@;�}@;]�@;�@:�@:͟@:�<@:��@:#:@9k�@8�f@8��@8u�@8z�@8��@8�4@8��@8��@8u�@8Ft@8 �@7�w@7��@7E9@6�@6�<@6{�@6e@5@5�@5m]@4��@4y>@41'@4,=@4�@3خ@3��@2�y@2�!@2xl@2B[@2�@2@1��@1��@1�@1ԕ@1p�@1+�@0��@0`�@0I�@0(�@/�;@/��@/�@.��@.Q@.?@.:*@.-@.($@-�"@-=�@-/@-�@,��@,�D@,��@,e�@,b@+��@+y�@+RT@+!-@*͟@*� @*C�@*&�@*J@*u@)�.@)�@)ԕ@)�z@)�~@)%@(�@(C-@(1'@(�@'�@'خ@'�Q@'خ@'�6@'�a@'t�@&��@&\�@&8�@&&�@%��@%�d@%�M@%X@%Vm@%N<@%?}@%%F@%�@$��@$,=@#�
@#��@#�V@#��@#~�@#v`@#X�@#=@"�s@"z@"e@!��@!�@!��@!s�@!&�@ �?@ �@ �o@ y>@ u�@ `�@ 4n@ ,=@ $@ %�@ ~@ @خ@��@�F@y�@
=@�@�b@	@�@�O@~(@A�@x@�m@��@x@�@�x@�@ԕ@�S@Y�@=�@+@�f@�v@��@~(@I�@?�@2�@~@�@�+@��@�:@j�@b�@b�@O@.I@�m@�@�T@�9@x�@`B@T�@7L@��@��@l"@�a@l�@e�@W?@F�@'�@�]@�X@�}@�x@�r@R�@E�@@ԕ@m]@�@�u@��@�o@z�@h�@9X@!@��@��@�[@��@��@iD@RT@O@;d@'�@��@ȴ@��@��@��@z@_�@e@�o@ԕ@�@��@��@�@�@�=@��@?}@V@�@�@ی@�j@�@r�@*�@��@��@��@��@�F@�@�f@l�@W?@6z@
=@
�@
��@
�@	�n@	c�@	=�@	&�@	+@��@��@��@�U@�O@�I@`�@~@��@˒@�V@|�@b�@K�@ߤ@~�@c @E�@	@�Z@�@�t@�h@Q�@IR@F@A @2a@��@��@bN@H@�@�:@Y@�@��@҉@��@~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AҿA�y�A�q�A�:�A�2-A�,=A�&LA�"4A��A�=A�1A��A�uA�4A�\A��A�
rA�
�A�	lA�fA�1A��A�+A��A���A���A��A��OAѹXAѱ�AѫkAѥ�Aї�A�K)A�QNAĥ�A���A�� A�u�A��@A�c�A�v�A�1A�>�A��HA�|�A���A�kQA���A�7LA��tA��?A�	�A��A��xA��RA��A��A���A�OA�{�A��=A�!bA��nA�SA���A���A�J#A��kA�FA��mA�w�A�=<A��A��A��	A�tTA��YA��A�t�A�DgA��BA��A��yA���A�~�A�,=A�m)A��lA�A�уA��A�?�A�^jA��HA�_A���A��gA���A��iA�3hA���A�f2A}��A|qA{��Az�Ax�)Av�AuB[Ar��ApzxAo��An�Al \AjW�Ah&Af�9Af+Ae��Ad�FAc'�A_˒A\�[AZ�AYK�AWOvAVl�AU�ATU2AS��AR�ARiDAQ�AQ\�AQ�AP"hAO�qAM�ALAK��AK(�AJ��AIAH
�AE�AE?ADzAC`�AB
�A@�pA?�[A>�A=��A< �A;5?A:��A:��A9��A7_A6�;A6-wA52aA4?}A3�MA3�eA32�A2�A1�A1u%A0��A/�jA/\�A/�A.ںA.��A.%A-s�A,/�A*YKA(��A(YA'VA&�'A&hsA&�A$�A#p;A"�pA!��A �6A _pA 	A�A�NAS&AJA� A�kAGA3�A�A�PA`BA%FA�#A��A \A}�A��AG�A�CAcA!�AԕACA��A�SAN<AA~�AA�AaAѷA
�A
�A	�$A	'�A��AC�A�3AxA�A�AY�A(A�A��AhsA�A��A��A _@��:@���@���@���@�V�@�r@���@�J�@�@�V@�}@�`B@��@�y>@�1�@貖@�c @�YK@�J#@��@�M@��@��@��.@୬@�	@߳�@�J#@���@ޥz@�%�@�q@��@�5�@�s�@�q�@�<�@��)@я�@Хz@ψf@�:�@͊	@���@��@��@ʩ�@ʣ�@ʑ�@�^5@��@Ƀ{@�!�@ȧ@�;�@ǽ�@�|�@�!�@�ی@�~(@�e@�y�@�w�@1@���@�0�@��@��c@���@�P�@��[@��O@��@��@��D@���@��@�O@���@�|�@�8�@���@�2�@��6@���@�`B@��@��8@��z@�v�@�c�@�]d@�L0@��@��N@�+k@�hs@�ߤ@��Y@�L�@�D�@��@���@��Z@��z@���@���@���@�e�@�6@���@��@��Q@��$@��F@��@�P�@��p@��@�M@��@��"@�V�@���@��@���@��`@���@��1@�}V@�l�@��@�f�@�Mj@�q@��9@�� @�PH@��6@�@�b@���@��^@���@�}�@�`B@�O�@�B�@�:�@�6z@�6z@�-w@��@�@@��@��5@��'@�-@��.@�W?@��,@��[@�I�@���@�Y�@��@��x@�u%@�;�@���@���@��@�4@���@� i@���@���@���@�y�@�f�@�T�@�
=@��@�-�@��@��N@��z@��@�m]@�U�@�8@�8@�#�@��@��5@��b@�($@��q@�K�@�"�@� i@��P@��2@��p@�-�@���@�a|@�($@�:�@�oi@�Xy@�I�@�1@���@���@�~�@�9�@��h@���@�U2@�	@~��@}Y�@}+�@|�@|��@|u�@|N�@|9X@|7�@|�@|1@{�m@{X�@{"�@{�@z�@z��@z�b@zu@y=�@x�@x�p@x��@xM@x*�@x�@v�s@v\�@v8�@u��@u�-@u�M@t�E@se�@r�h@r#:@q��@q�D@q��@p�@p��@pz�@o�W@oA�@n�y@n��@nV@n	@m�@ne@n^5@n3�@n@m��@l֡@l~(@lz�@lj@l~@lx@k��@k��@ky�@kY@j�@j�'@j��@i�@h�4@h��@h�4@h�z@h�@hg8@h%�@f��@f��@f�@f�8@f�@f��@f��@fR�@e�Z@e�h@eX@d��@d7�@c˒@c�@c�@a�@a8�@a \@aV@a�@`�$@`S�@`A�@`�@_��@_��@_��@_>�@^�1@^��@^�A@^d�@^:*@]�o@]�@]s�@\��@[�6@[C@Z�X@Z:*@Yϫ@Y��@Y��@Y��@Ym]@YL�@Y2a@Y;@X�@Xی@XɆ@X��@X�@X��@X��@Xr�@Xe�@XH@W|�@W@O@V�c@V�'@V�@T�O@Tz�@TI�@T9X@T%�@T1@S�@@S�4@SdZ@S)_@R��@R�<@R��@R�@Rh
@Q�j@Q@Q�M@Q�M@Q/@P:�@O��@O�@N�A@N\�@N?@N)�@N�@N4@M�@M��@M4@L�5@Lm�@K��@K��@K\)@J��@J�H@J��@JYK@J)�@J�@I�@I�Z@I��@I�@I}�@IIR@I=�@I7L@Hی@Hq@H[�@G�:@G i@F�<@Fh
@E�T@Em]@E@D�[@D��@DtT@D"h@C��@B�m@BV@A��@A��@A��@Ac@Ak�@A[W@A8�@@ѷ@@m�@?��@?=@?�@?�@>�H@>�6@>L0@>O@=�Z@=��@<��@<oi@;�}@;]�@;�@:�@:͟@:�<@:��@:#:@9k�@8�f@8��@8u�@8z�@8��@8�4@8��@8��@8u�@8Ft@8 �@7�w@7��@7E9@6�@6�<@6{�@6e@5@5�@5m]@4��@4y>@41'@4,=@4�@3خ@3��@2�y@2�!@2xl@2B[@2�@2@1��@1��@1�@1ԕ@1p�@1+�@0��@0`�@0I�@0(�@/�;@/��@/�@.��@.Q@.?@.:*@.-@.($@-�"@-=�@-/@-�@,��@,�D@,��@,e�@,b@+��@+y�@+RT@+!-@*͟@*� @*C�@*&�@*J@*u@)�.@)�@)ԕ@)�z@)�~@)%@(�@(C-@(1'@(�@'�@'خ@'�Q@'خ@'�6@'�a@'t�@&��@&\�@&8�@&&�@%��@%�d@%�M@%X@%Vm@%N<@%?}@%%F@%�@$��@$,=@#�
@#��@#�V@#��@#~�@#v`@#X�@#=@"�s@"z@"e@!��@!�@!��@!s�@!&�@ �?@ �@ �o@ y>@ u�@ `�@ 4n@ ,=@ $@ %�@ ~@ @خ@��@�F@y�@
=@�@�b@	@�@�O@~(@A�@x@�m@��@x@�@�x@�@ԕ@�S@Y�@=�@+@�f@�v@��@~(@I�@?�@2�@~@�@�+@��@�:@j�@b�@b�@O@.I@�m@�@�T@�9@x�@`B@T�@7L@��@��@l"@�a@l�@e�@W?@F�@'�@�]@�X@�}@�x@�r@R�@E�@@ԕ@m]@�@�u@��@�o@z�@h�@9X@!@��@��@�[@��@��@iD@RT@O@;d@'�@��@ȴ@��@��@��@z@_�@e@�o@ԕ@�@��@��@�@�@�=@��@?}@V@�@�@ی@�j@�@r�@*�@��@��@��@��@�F@�@�f@l�@W?@6z@
=@
�@
��@
�@	�n@	c�@	=�@	&�@	+@��@��@��@�U@�O@�I@`�@~@��@˒@�V@|�@b�@K�@ߤ@~�@c @E�@	@�Z@�@�t@�h@Q�@IR@F@A @2a@��@��@bN@H@�@�:@Y@�@��@҉@��@~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�=B�1B��B�1B�_B�B�EB�EB�+B��B�B�B�B�B��B��B��B��B��B��B��B�B��B�B�+B�%B��B��B��B��B��B��B��B|�BK�B�B��B��B��B��B�OB��B��B�tBu�Be�Bb�B_�Bb�BX�BW�BRBI�BAoB@�BB�B:�B7�B3�B&�B5BMB�B)B�B��B��B�)B��B�PB�"B�2B�aB�5B�yB��B}VBm�Be�B\�BN�B9�B,=B)yB&�B"NB�B�B�B߾B�B��B�B��B�{B��B~(Bq�Bd�BV�BJ�BA B5�B vB�B.B	�B
�B
�'B
�B
ݘB
�B
��B
�0B
�IB
��B
�,B
�	B
��B
~wB
x�B
l�B
bNB
OBB
H1B
>�B
8B
2�B
/�B
)_B
$�B
 \B
�B
B
QB
sB
�B
B
HB
	7B
�B
3B
�B
�B	�BB	�B	�nB	�B	�QB	��B	޸B	�QB	�YB	�B	�B	��B	��B	�"B	��B	�[B	�B	��B	�DB	��B	�@B	��B	��B	�5B	��B	�
B	��B	��B	�6B	�B	�B	�?B	�iB	zB	oB	d@B	_;B	Z�B	U�B	RTB	OB	N<B	J#B	ESB	B�B	?cB	<�B	:B	9rB	7�B	7fB	2�B	2B	1B	0oB	/B	.�B	*eB	(�B	($B	&�B	%`B	$@B	�B	)B	�B	?B	�B	�B	�B	TB	hB	�B	�B	B	�B	B	~B	
�B		B		B	
	B	�B	B	B	9B	B	�B	aB	�B	�B	�B	 iB	  B��B��B��B��B��B�B�MB�-B��B��B�;B��B��B�}B�B�CB�qB��B�qB��B�kB��B��B�B��B��B�eB��B��B��B�B�B�6B�B�B�6B�QB��B�]B�IB��B�B�+B��B�B��B��B�xB��B��B�wB��B��B��B��B�.B��B	 4B	 �B	;B	�B	AB	B	�B	�B	B	�B	�B	3B	B	�B	�B	B	1B	�B	�B	�B	,B	�B	�B	�B	]B	IB	�B	 BB	"NB	(�B	/ B	0UB	0UB	0�B	3hB	5�B	72B	:xB	<jB	=<B	=�B	?.B	A;B	A�B	F�B	HfB	L�B	RoB	Z�B	_B	_!B	_pB	_;B	_pB	_;B	_VB	_;B	_VB	_pB	eFB	h>B	`�B	Z�B	\�B	_�B	b�B	e`B	h>B	kQB	ncB	o5B	p;B	utB	xRB	yrB	z�B	~�B	�B	�B	��B	��B	��B	�\B	��B	�(B	��B	��B	��B	�B	��B	�B	��B	�OB	��B	��B	��B	��B	�BB	�\B	�\B	��B	�bB	��B	��B	��B	�zB	��B	�eB	�B	��B	�hB	��B	�hB	��B	�zB	�rB	�*B	��B	�B	��B	��B	ȀB	�)B	ϫB	ڠB	�/B	ݘB	ޞB	ߊB	�B	� B	�B	�kB	�B	� B	��B	�B	�B	�?B	�2B	�2B	�B	�rB	�dB	��B
�B
	B
�B
�B
�B
�B
xB
DB
�B
�B
�B
�B
�B
B
sB
sB
B
�B
 vB
"�B
#�B
%�B
&B
'B
'�B
.B
0UB
0UB
1B
1�B
2B
2aB
2aB
2|B
2�B
3B
3hB
5ZB
5�B
6B
6FB
6B
7LB
9	B
;�B
>�B
@�B
BuB
E9B
E�B
E�B
J�B
N<B
O�B
Q�B
R�B
S�B
T�B
XyB
YB
YB
Y�B
YeB
Y�B
ZQB
Z7B
Z�B
]IB
`�B
cB
d�B
e�B
ffB
gmB
hsB
jKB
j�B
j�B
j�B
k�B
lB
k�B
l"B
l�B
l�B
nIB
n�B
n�B
p�B
q�B
q�B
r�B
vB
|�B
}B
}B
}VB
}�B
B
��B
�zB
��B
��B
��B
��B
��B
�RB
�DB
��B
�B
��B
��B
�TB
��B
�B
�B
��B
��B
�B
�]B
�B
�B
�5B
�OB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�TB
�&B
��B
�B
�8B
��B
��B
��B
�B
�/B
�}B
��B
��B
��B
��B
�!B
��B
��B
�B
�[B
�AB
�AB
�[B
��B
�aB
�|B
��B
��B
�B
��B
��B
��B
��B
�B
�jB
��B
��B
��B
��B
�(B
�]B
�B
�}B
�OB
��B
��B
�UB
��B
�-B
ÖB
�aB
�3B
��B
��B
ɆB
��B
�DB
˒B
��B
��B
��B
�JB
�B
�B
οB
�B
�NB
уB
ңB
�[B
ӏB
��B
��B
՛B
��B
�B
�B
�B
�9B
�sB
��B
��B
��B
��B
��B
��B
��B
�/B
ݘB
�jB
�;B
�'B
��B
�|B
��B
�4B
�B
��B
�B
��B
�B
�$B
�XB
�XB
�B
�B
��B
�B
�0B
�B
�B
��B
��B
�CB
��B
�}B
�B
� B
�B
��B
��B
��B
�B
�9B
��B
��B
��B
��B
��B
��B
��B
�lB
��B
��B
��B
��B
��B
�lB
��B
��B
�>B
��B
�*B
��B
�0B
�dB
��B
�jB
��B
��B
�<B
�(B
��B
�cB
�HB
�}B
��B BUBoB�BBAB[BuBuB�BuBaB{B�B�B�BB�B�BtB_B�B�B�B�B�B	7B	�B	�B	�B
XB
rB
rB
�BDB�B�B0BdB�B6B�B�B"B"B"B"BpBpB�B\BbB}B�B�B4B4BBB4B4B�B�B@BuB[B�B�BaB�B�B�B�B�B�BMBSB�B�B�B
B$B$B?BYB�B_B�B1BeB�B�BB�B�B	B	B#B=BqBqB�BqB�B�BB�BBxB�BBBB�B�B BB �B �B �B!-B!|B"NB"�B#TB#�B#�B$ZB$tB$�B$�B$�B%,B%zB%�B%�B%�B%�B%�B&B&2B&�B&�B&�B&�B&�B'B'�B(�B(�B(�B)yB)yB)yB)�B)�B)�B*�B+�B,B,B,"B,"B,qB,�B,�B-B-B-CB-�B-�B-�B-�B.�B/�B/�B/�B/�B/�B0B0;B0UB0�B0�B1B1B1'B1vB1vB1[B1�B1�B1�B2-B2aB2|B2�B2�B2�B33B3�B3�B3�B3�B3�B3�B3�B3�B4B4TB4�B4�B4�B4�B5B5%B5tB5�B6+B6`B6`B6zB6zB6�B6�B6�B6�B7B7LB7LB7�B8�B9$B9�B9�B9�B9�B:B:B:*B:^B:^B:xB:�B;B;JB;�B;�B<B<B<B<�B=<B=qB=�B=�B>B>(B>]B>�B>�B>�B>�B>�B>�B?�B?�B@B@B@iBAUBBBA�BB'BB[BB�BC-4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B�=B�1B��B�1B�_B�B�EB�EB�+B��B�B�B�B�B��B��B��B��B��B��B��B�B��B�B�+B�%B��B��B��B��B��B��B��B|�BK�B�B��B��B��B��B�OB��B��B�tBu�Be�Bb�B_�Bb�BX�BW�BRBI�BAoB@�BB�B:�B7�B3�B&�B5BMB�B)B�B��B��B�)B��B�PB�"B�2B�aB�5B�yB��B}VBm�Be�B\�BN�B9�B,=B)yB&�B"NB�B�B�B߾B�B��B�B��B�{B��B~(Bq�Bd�BV�BJ�BA B5�B vB�B.B	�B
�B
�'B
�B
ݘB
�B
��B
�0B
�IB
��B
�,B
�	B
��B
~wB
x�B
l�B
bNB
OBB
H1B
>�B
8B
2�B
/�B
)_B
$�B
 \B
�B
B
QB
sB
�B
B
HB
	7B
�B
3B
�B
�B	�BB	�B	�nB	�B	�QB	��B	޸B	�QB	�YB	�B	�B	��B	��B	�"B	��B	�[B	�B	��B	�DB	��B	�@B	��B	��B	�5B	��B	�
B	��B	��B	�6B	�B	�B	�?B	�iB	zB	oB	d@B	_;B	Z�B	U�B	RTB	OB	N<B	J#B	ESB	B�B	?cB	<�B	:B	9rB	7�B	7fB	2�B	2B	1B	0oB	/B	.�B	*eB	(�B	($B	&�B	%`B	$@B	�B	)B	�B	?B	�B	�B	�B	TB	hB	�B	�B	B	�B	B	~B	
�B		B		B	
	B	�B	B	B	9B	B	�B	aB	�B	�B	�B	 iB	  B��B��B��B��B��B�B�MB�-B��B��B�;B��B��B�}B�B�CB�qB��B�qB��B�kB��B��B�B��B��B�eB��B��B��B�B�B�6B�B�B�6B�QB��B�]B�IB��B�B�+B��B�B��B��B�xB��B��B�wB��B��B��B��B�.B��B	 4B	 �B	;B	�B	AB	B	�B	�B	B	�B	�B	3B	B	�B	�B	B	1B	�B	�B	�B	,B	�B	�B	�B	]B	IB	�B	 BB	"NB	(�B	/ B	0UB	0UB	0�B	3hB	5�B	72B	:xB	<jB	=<B	=�B	?.B	A;B	A�B	F�B	HfB	L�B	RoB	Z�B	_B	_!B	_pB	_;B	_pB	_;B	_VB	_;B	_VB	_pB	eFB	h>B	`�B	Z�B	\�B	_�B	b�B	e`B	h>B	kQB	ncB	o5B	p;B	utB	xRB	yrB	z�B	~�B	�B	�B	��B	��B	��B	�\B	��B	�(B	��B	��B	��B	�B	��B	�B	��B	�OB	��B	��B	��B	��B	�BB	�\B	�\B	��B	�bB	��B	��B	��B	�zB	��B	�eB	�B	��B	�hB	��B	�hB	��B	�zB	�rB	�*B	��B	�B	��B	��B	ȀB	�)B	ϫB	ڠB	�/B	ݘB	ޞB	ߊB	�B	� B	�B	�kB	�B	� B	��B	�B	�B	�?B	�2B	�2B	�B	�rB	�dB	��B
�B
	B
�B
�B
�B
�B
xB
DB
�B
�B
�B
�B
�B
B
sB
sB
B
�B
 vB
"�B
#�B
%�B
&B
'B
'�B
.B
0UB
0UB
1B
1�B
2B
2aB
2aB
2|B
2�B
3B
3hB
5ZB
5�B
6B
6FB
6B
7LB
9	B
;�B
>�B
@�B
BuB
E9B
E�B
E�B
J�B
N<B
O�B
Q�B
R�B
S�B
T�B
XyB
YB
YB
Y�B
YeB
Y�B
ZQB
Z7B
Z�B
]IB
`�B
cB
d�B
e�B
ffB
gmB
hsB
jKB
j�B
j�B
j�B
k�B
lB
k�B
l"B
l�B
l�B
nIB
n�B
n�B
p�B
q�B
q�B
r�B
vB
|�B
}B
}B
}VB
}�B
B
��B
�zB
��B
��B
��B
��B
��B
�RB
�DB
��B
�B
��B
��B
�TB
��B
�B
�B
��B
��B
�B
�]B
�B
�B
�5B
�OB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�TB
�&B
��B
�B
�8B
��B
��B
��B
�B
�/B
�}B
��B
��B
��B
��B
�!B
��B
��B
�B
�[B
�AB
�AB
�[B
��B
�aB
�|B
��B
��B
�B
��B
��B
��B
��B
�B
�jB
��B
��B
��B
��B
�(B
�]B
�B
�}B
�OB
��B
��B
�UB
��B
�-B
ÖB
�aB
�3B
��B
��B
ɆB
��B
�DB
˒B
��B
��B
��B
�JB
�B
�B
οB
�B
�NB
уB
ңB
�[B
ӏB
��B
��B
՛B
��B
�B
�B
�B
�9B
�sB
��B
��B
��B
��B
��B
��B
��B
�/B
ݘB
�jB
�;B
�'B
��B
�|B
��B
�4B
�B
��B
�B
��B
�B
�$B
�XB
�XB
�B
�B
��B
�B
�0B
�B
�B
��B
��B
�CB
��B
�}B
�B
� B
�B
��B
��B
��B
�B
�9B
��B
��B
��B
��B
��B
��B
��B
�lB
��B
��B
��B
��B
��B
�lB
��B
��B
�>B
��B
�*B
��B
�0B
�dB
��B
�jB
��B
��B
�<B
�(B
��B
�cB
�HB
�}B
��B BUBoB�BBAB[BuBuB�BuBaB{B�B�B�BB�B�BtB_B�B�B�B�B�B	7B	�B	�B	�B
XB
rB
rB
�BDB�B�B0BdB�B6B�B�B"B"B"B"BpBpB�B\BbB}B�B�B4B4BBB4B4B�B�B@BuB[B�B�BaB�B�B�B�B�B�BMBSB�B�B�B
B$B$B?BYB�B_B�B1BeB�B�BB�B�B	B	B#B=BqBqB�BqB�B�BB�BBxB�BBBB�B�B BB �B �B �B!-B!|B"NB"�B#TB#�B#�B$ZB$tB$�B$�B$�B%,B%zB%�B%�B%�B%�B%�B&B&2B&�B&�B&�B&�B&�B'B'�B(�B(�B(�B)yB)yB)yB)�B)�B)�B*�B+�B,B,B,"B,"B,qB,�B,�B-B-B-CB-�B-�B-�B-�B.�B/�B/�B/�B/�B/�B0B0;B0UB0�B0�B1B1B1'B1vB1vB1[B1�B1�B1�B2-B2aB2|B2�B2�B2�B33B3�B3�B3�B3�B3�B3�B3�B3�B4B4TB4�B4�B4�B4�B5B5%B5tB5�B6+B6`B6`B6zB6zB6�B6�B6�B6�B7B7LB7LB7�B8�B9$B9�B9�B9�B9�B:B:B:*B:^B:^B:xB:�B;B;JB;�B;�B<B<B<B<�B=<B=qB=�B=�B>B>(B>]B>�B>�B>�B>�B>�B>�B?�B?�B@B@B@iBAUBBBA�BB'BB[BB�BC-4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221025034227  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221025034236  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221025034236  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221025034237                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221025124241  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221025124241  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20221025040206                      G�O�G�O�G�O�                
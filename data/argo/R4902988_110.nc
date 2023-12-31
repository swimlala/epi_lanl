CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-10-15T03:40:53Z creation;2022-10-15T03:40:54Z conversion to V3.1      
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20221015034053  20221015035843  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               nA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��A��W1   @��A� �.@:�������c�-V1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A�  A�  A�33A�33A�  A���A�  B   B��B��B��B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C�fC�fC  C�C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C��3C��C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C�  C�  C�  C�  C��C��C�  C�  C�  C��3C��3C��3C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C��3C��3C��3C�  C�  C��C��C�  C��C�  C��3C�  D fD �fD  D� D  Dy�D��D� D  D� D  D� D  Dy�D  D� D  Dy�D��D	� D
fD
� D  D� D  D�fDfD�fD  D� D  D� D  Dy�D��D� D  D� D  D�fD  D� D  D� D  D�fD  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DXy�DX��DY� DZ  DZ� D[  D[�fD\  D\� D\��D]� D^fD^� D^��D_� D`  D`�fDa  Da� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dyy�Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�|�D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D��3D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�3D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�3D�@ D�� D���D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ Dȼ�D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D˼�D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D҃3D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�C3DՀ D�� D�  D�@ Dր D�� D�3D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ Dڼ�D�  D�@ Dۀ D�� D���D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�<�D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�\)@�\)A�A?�A_�A�A��
A��
A�
=A�
=A��
A��A��
A��
B�B�B�B�B'�B0Q�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B�(�B���B���B���B���B�(�B���B���B���B���B���B���B���B�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	�GC��C��C��C�GC�GC��C{C��C��C��C��C!��C#�GC%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CL{CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cv{Cw��Cy��C{��C}��C��C��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC�
>C��qC�
>C��qC��qC��qC��qC��qC��C�
>C�
>C��qC��qC��qC��qC��qC��C��qC��qC��C��qC�
>C�
>C��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��C��C��C��qC��C��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��C��qC��qC��qC��C��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��C��qC��qC��qC��C��C��C��qC��qC�
>C�
>C��qC�
>C��qC��C��qD D �D ��D~�D��DxRD�RD~�D��D~�D��D~�D��DxRD��D~�D��DxRD�RD	~�D
D
~�D
��D~�D��D�DD�D��D~�D��D~�D��DxRD�RD~�D��D~�D��D�D��D~�D��D~�D��D�D��D~�D��D~�D�RDxRD��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D-D-~�D-��D.~�D.��D/~�D/��D0~�D0�RD1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5�RD6~�D6��D7~�D7��D8~�D8�RD9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>xRD>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DFxRDF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DMxRDM��DN~�DN��DO~�DO��DP~�DP��DQ~�DRDR~�DR��DS~�DS��DT~�DT��DU~�DU�RDV~�DV��DW~�DW��DXxRDX�RDY~�DY��DZ~�DZ��D[�D[��D\~�D\�RD]~�D^D^~�D^�RD_~�D_��D`�D`��Da~�Da��Db~�Db�RDc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�DpDp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��DyxRDy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��)D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�|)D��\D��\D�?\D�|)D��)D��\D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�B�D�\D�D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�|)D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D���D�D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D�D��\D�?\D�\D�D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��)D�?\D�\D��\D��D�B�D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�?\D�\D��\D��)D�?\D�\D��\D��D�?\D�\D��)D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȼ)D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˼)D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D҂�Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�B�D�\Dտ\D��\D�?\D�\Dֿ\D��D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڼ)D��\D�?\D�\Dۿ\D��)D�<)D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��D�B�D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��)D�<)D�\D�\D��\D�?\D�\D�\D��\D�<)D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�)D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�B�D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�x8A�x8A�w�A�xA�w�A�p�A�o A�ncA�a�A�J�A�@�A�>�A�;0A�:*A�:*A�9$A�2-A�/�A�-�A�*�A�-wA�+6A� 'A���A�v`A��BA�p�A���A���Aɜ�A���AƢ�A��jA�چA��NA�C�A��vA�q�A�"�A���A���A��5A�l"A�1�A���A��A�.A�<�A�1�A�]�A��BA��uA���A��A�:�A�/�A�l�A�;0A�"hA��A��eA�L�A��A���A�VA�h
A��NA���A��A���A�#�A��VA���A�cA�4nA���A��6A�A�� A���A�<jA�MA��TA�� A�V�A���A��=A��A�YA��A�g�A��A�}�A���A��A�EmA��A��@A�y�A�5tA��A��A�5�A���A���A�5A��kA�;dA���A�˒A��:Ap;A~�cA}��A|�Ay�6Ax��Aw��AwL0Av�_AuuAs��Ar��Aqu�An�SAlp;Ai�PAe�AcYA`��A_GEA^!A]�AZ��AXg�AT~�AR��AQ�'AP�AP-wAOoiAN��AN�AMQ�AL�AJkQAI��AI�RAH�AF�AEs�AD{JADH�AC��AC�MAC=qAB�NABoAA&�A@��A@��A@y>A?�A?�A>�oA>cA=��A=MA<�_A;�UA;A A;A:��A9�A91�A8~�A7A�A7�A6��A6RTA5�A53�A4��A4m]A4?}A3oiA1��A0��A0�A0]dA0JA/��A/p;A.Z�A-qA*��A(iDA'OA&�A&��A&W?A%��A$[�A"J�A OvAE�A�:A+A�A�A�A��A-�AW?Av�A�ZAC-AS&A�A�A�A4A�A�A��A�+A�A'RA
��A
4A	��A	�uA	�A��AdZA��A'RA�HA@OA��A�A`�A�PAS&A "h@�@��@��@��@��@��@�Y@�@�*�@��q@�O@쉠@��@��@ꉠ@�f�@��+@�@��@�=@ᙚ@��@�m]@޸R@�W?@۴�@��@�c�@��|@ؖ�@�@�l"@�n�@�,�@Ѯ�@�#�@Ќ�@α�@��j@ͤ@@�[W@���@��`@̦L@�U2@�?@���@��@�j�@��@���@�(�@ƿ�@�:�@��.@�4@��@��U@�J@���@��H@��@�Z@���@�`B@�_�@�n/@���@�s�@�*�@�|�@���@��@�!-@��X@��@��\@��@�m]@�+�@���@��@�  @���@�YK@��H@�RT@��@�	@��@�Z�@���@���@�@�Vm@�tT@��@�$t@�֡@�,=@�J#@��R@�9X@�n/@��'@�.�@�ԕ@��C@��@��{@��{@���@�w2@�4�@���@��2@���@�:*@���@�@��T@��"@�~�@�_p@��K@���@�h
@��<@���@��=@�ی@�I�@���@�O�@��@�2�@��.@�ϫ@��7@�qv@�dZ@�@O@�'�@���@���@���@�q@�s�@�p;@�h
@�_�@�Ov@��@���@�{J@�)_@���@��?@���@���@��+@�q�@�a|@�bN@�_@�\�@�V�@�J�@�:�@��@��f@�B�@��f@�:�@��@���@���@���@��A@�u�@�C-@�2�@�,=@�1'@�6@�8�@�-�@�
�@��]@��]@��Z@��W@��>@��@�ݘ@��g@��N@���@��0@��@���@���@���@��n@���@�+@�W�@�	l@���@���@���@���@��7@��@�RT@�Y@���@��2@�҉@�ȴ@���@��e@���@���@���@�Xy@�e@���@��P@�c�@~�@~�r@}�D@|�`@|  @z��@y�N@yu�@y%F@x֡@x�@xl"@x@w�Q@w�@wn/@w>�@v�@v��@v&�@u��@u��@uu�@u-w@t��@s�@sA�@rȴ@q�j@p��@p��@p��@p[�@pK^@pU2@p:�@ox@n�L@n~�@nYK@m�D@m@m��@mk�@mO�@mS&@m2a@m;@l�`@l��@l�@l�Y@lc�@lPH@l9X@lM@k��@k�@@k4�@j�'@i�t@i�@h�9@hj@h1'@g�&@g�@e�h@e%@d��@d��@dZ@dx@ciD@b�c@bs�@b
�@a�@a5�@`�@^�8@^��@^�}@^�@^Z�@^0U@]�N@]|@]^�@]:�@]%F@\�	@\�e@\��@\r�@\q@\2�@\M@[�[@[/�@Z�B@Zp;@Y[W@X �@W��@W�*@W��@W�@W�@Wb�@W]�@WU�@WMj@WE9@W8@W$t@Wo@Vߤ@V�@V��@V��@V��@V�A@Vz@V~�@V��@VJ�@V($@U�@T��@TD�@S��@TV�@TS�@T/�@R6�@QS&@QO�@Q/@Q�@QV@P~@Nu%@M��@Mj@M8�@M@L�@L�[@L��@L~(@LM@L/�@L~@L@L�@K�m@K��@K@O@J�1@J0U@IV@H�u@Hc�@H6@H�@G�+@G�0@G\)@F�@F_�@F+k@F4@E�@E��@E�z@E��@E��@E�C@E��@E�=@E�7@E<6@Dr�@De�@DZ@DM@C�@Cl�@C i@Bh
@Bd�@B^5@B@�@A�@A��@Ap�@A8�@A	l@@�|@@��@@�u@@�Y@@_@@:�@@(�@?��@>R�@=��@=zx@==�@<�@;�W@;�*@;�@::*@9��@9�@8��@8��@8]d@81@7خ@7˒@7�w@7��@7W?@7�@6YK@5�#@5�X@5J�@5=�@5(�@4��@4��@4�@4��@4]d@4$@4~@4�@4�@3�A@3�F@3a@3E9@3$t@2�M@2�@2��@2l�@2{@1��@1rG@1/@1;@0�@/��@/�@.�@.��@.@�@.$�@-ԕ@-��@-Vm@,��@,�@,�D@,z�@,oi@,4n@+�@+l�@+C@+@*��@*��@*��@*��@*��@*L0@)ԕ@)��@)x�@)f�@):�@) \@(��@(�`@(�p@(�@(�@(�o@(tT@(Z@(2�@(2�@(6@(1'@(@'�[@'e�@'4�@'�@&��@&�2@&��@&�s@&�X@&�<@&u%@&�@%@%x�@%f�@%8�@$Ɇ@$��@$�.@$u�@$7�@$1@#��@#y�@#.I@"�b@"{@!�@!O�@!+�@ ��@ �p@ ��@ �o@ [�@ "h@ 	�@   @�@�@l�@�@�h@h
@�D@�n@Dg@+�@&�@@@��@ی@�)@�@w�@Z@I�@4n@�W@�a@e�@�L@s�@h
@\�@E�@8�@+k@!�@4@�z@u�@��@2�@��@��@y�@j�@,�@�@�6@��@��@��@z@a|@+k@��@��@X@�@�5@֡@g8@��@o@�X@��@� @}V@l�@L0@�@��@S&@Dg@!�@�@��@�@�/@�j@�_@`�@�@��@�a@��@�4@X�@\)@U�@RT@X�@_p@]�@]�@K�@�@�m@+k@�@�@�z@��@�H@�^@��@�C@�@|@a�@Dg@@@@@@�$@��@�u@]d@�@�6@��@�:@g�@W?@4�@C@�@
�y@
�'@
�L@
xl@
&�@	�@	��@	F@	�@�/@�[@��@�O@��@�I@m�@Xy@H@>B@,=@�@�]@�w@x@�'@^5@;�@	@�@@�>@��@�9@@��@��@�M@a�@V@�@��@�U@�_@e�@@�@� @�@�w@��@��@�@n/@;d@&@
=@��@��@u%@�@@��@�o@�#@�~@f�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�x8A�x8A�w�A�xA�w�A�p�A�o A�ncA�a�A�J�A�@�A�>�A�;0A�:*A�:*A�9$A�2-A�/�A�-�A�*�A�-wA�+6A� 'A���A�v`A��BA�p�A���A���Aɜ�A���AƢ�A��jA�چA��NA�C�A��vA�q�A�"�A���A���A��5A�l"A�1�A���A��A�.A�<�A�1�A�]�A��BA��uA���A��A�:�A�/�A�l�A�;0A�"hA��A��eA�L�A��A���A�VA�h
A��NA���A��A���A�#�A��VA���A�cA�4nA���A��6A�A�� A���A�<jA�MA��TA�� A�V�A���A��=A��A�YA��A�g�A��A�}�A���A��A�EmA��A��@A�y�A�5tA��A��A�5�A���A���A�5A��kA�;dA���A�˒A��:Ap;A~�cA}��A|�Ay�6Ax��Aw��AwL0Av�_AuuAs��Ar��Aqu�An�SAlp;Ai�PAe�AcYA`��A_GEA^!A]�AZ��AXg�AT~�AR��AQ�'AP�AP-wAOoiAN��AN�AMQ�AL�AJkQAI��AI�RAH�AF�AEs�AD{JADH�AC��AC�MAC=qAB�NABoAA&�A@��A@��A@y>A?�A?�A>�oA>cA=��A=MA<�_A;�UA;A A;A:��A9�A91�A8~�A7A�A7�A6��A6RTA5�A53�A4��A4m]A4?}A3oiA1��A0��A0�A0]dA0JA/��A/p;A.Z�A-qA*��A(iDA'OA&�A&��A&W?A%��A$[�A"J�A OvAE�A�:A+A�A�A�A��A-�AW?Av�A�ZAC-AS&A�A�A�A4A�A�A��A�+A�A'RA
��A
4A	��A	�uA	�A��AdZA��A'RA�HA@OA��A�A`�A�PAS&A "h@�@��@��@��@��@��@�Y@�@�*�@��q@�O@쉠@��@��@ꉠ@�f�@��+@�@��@�=@ᙚ@��@�m]@޸R@�W?@۴�@��@�c�@��|@ؖ�@�@�l"@�n�@�,�@Ѯ�@�#�@Ќ�@α�@��j@ͤ@@�[W@���@��`@̦L@�U2@�?@���@��@�j�@��@���@�(�@ƿ�@�:�@��.@�4@��@��U@�J@���@��H@��@�Z@���@�`B@�_�@�n/@���@�s�@�*�@�|�@���@��@�!-@��X@��@��\@��@�m]@�+�@���@��@�  @���@�YK@��H@�RT@��@�	@��@�Z�@���@���@�@�Vm@�tT@��@�$t@�֡@�,=@�J#@��R@�9X@�n/@��'@�.�@�ԕ@��C@��@��{@��{@���@�w2@�4�@���@��2@���@�:*@���@�@��T@��"@�~�@�_p@��K@���@�h
@��<@���@��=@�ی@�I�@���@�O�@��@�2�@��.@�ϫ@��7@�qv@�dZ@�@O@�'�@���@���@���@�q@�s�@�p;@�h
@�_�@�Ov@��@���@�{J@�)_@���@��?@���@���@��+@�q�@�a|@�bN@�_@�\�@�V�@�J�@�:�@��@��f@�B�@��f@�:�@��@���@���@���@��A@�u�@�C-@�2�@�,=@�1'@�6@�8�@�-�@�
�@��]@��]@��Z@��W@��>@��@�ݘ@��g@��N@���@��0@��@���@���@���@��n@���@�+@�W�@�	l@���@���@���@���@��7@��@�RT@�Y@���@��2@�҉@�ȴ@���@��e@���@���@���@�Xy@�e@���@��P@�c�@~�@~�r@}�D@|�`@|  @z��@y�N@yu�@y%F@x֡@x�@xl"@x@w�Q@w�@wn/@w>�@v�@v��@v&�@u��@u��@uu�@u-w@t��@s�@sA�@rȴ@q�j@p��@p��@p��@p[�@pK^@pU2@p:�@ox@n�L@n~�@nYK@m�D@m@m��@mk�@mO�@mS&@m2a@m;@l�`@l��@l�@l�Y@lc�@lPH@l9X@lM@k��@k�@@k4�@j�'@i�t@i�@h�9@hj@h1'@g�&@g�@e�h@e%@d��@d��@dZ@dx@ciD@b�c@bs�@b
�@a�@a5�@`�@^�8@^��@^�}@^�@^Z�@^0U@]�N@]|@]^�@]:�@]%F@\�	@\�e@\��@\r�@\q@\2�@\M@[�[@[/�@Z�B@Zp;@Y[W@X �@W��@W�*@W��@W�@W�@Wb�@W]�@WU�@WMj@WE9@W8@W$t@Wo@Vߤ@V�@V��@V��@V��@V�A@Vz@V~�@V��@VJ�@V($@U�@T��@TD�@S��@TV�@TS�@T/�@R6�@QS&@QO�@Q/@Q�@QV@P~@Nu%@M��@Mj@M8�@M@L�@L�[@L��@L~(@LM@L/�@L~@L@L�@K�m@K��@K@O@J�1@J0U@IV@H�u@Hc�@H6@H�@G�+@G�0@G\)@F�@F_�@F+k@F4@E�@E��@E�z@E��@E��@E�C@E��@E�=@E�7@E<6@Dr�@De�@DZ@DM@C�@Cl�@C i@Bh
@Bd�@B^5@B@�@A�@A��@Ap�@A8�@A	l@@�|@@��@@�u@@�Y@@_@@:�@@(�@?��@>R�@=��@=zx@==�@<�@;�W@;�*@;�@::*@9��@9�@8��@8��@8]d@81@7خ@7˒@7�w@7��@7W?@7�@6YK@5�#@5�X@5J�@5=�@5(�@4��@4��@4�@4��@4]d@4$@4~@4�@4�@3�A@3�F@3a@3E9@3$t@2�M@2�@2��@2l�@2{@1��@1rG@1/@1;@0�@/��@/�@.�@.��@.@�@.$�@-ԕ@-��@-Vm@,��@,�@,�D@,z�@,oi@,4n@+�@+l�@+C@+@*��@*��@*��@*��@*��@*L0@)ԕ@)��@)x�@)f�@):�@) \@(��@(�`@(�p@(�@(�@(�o@(tT@(Z@(2�@(2�@(6@(1'@(@'�[@'e�@'4�@'�@&��@&�2@&��@&�s@&�X@&�<@&u%@&�@%@%x�@%f�@%8�@$Ɇ@$��@$�.@$u�@$7�@$1@#��@#y�@#.I@"�b@"{@!�@!O�@!+�@ ��@ �p@ ��@ �o@ [�@ "h@ 	�@   @�@�@l�@�@�h@h
@�D@�n@Dg@+�@&�@@@��@ی@�)@�@w�@Z@I�@4n@�W@�a@e�@�L@s�@h
@\�@E�@8�@+k@!�@4@�z@u�@��@2�@��@��@y�@j�@,�@�@�6@��@��@��@z@a|@+k@��@��@X@�@�5@֡@g8@��@o@�X@��@� @}V@l�@L0@�@��@S&@Dg@!�@�@��@�@�/@�j@�_@`�@�@��@�a@��@�4@X�@\)@U�@RT@X�@_p@]�@]�@K�@�@�m@+k@�@�@�z@��@�H@�^@��@�C@�@|@a�@Dg@@@@@@�$@��@�u@]d@�@�6@��@�:@g�@W?@4�@C@�@
�y@
�'@
�L@
xl@
&�@	�@	��@	F@	�@�/@�[@��@�O@��@�I@m�@Xy@H@>B@,=@�@�]@�w@x@�'@^5@;�@	@�@@�>@��@�9@@��@��@�M@a�@V@�@��@�U@�_@e�@@�@� @�@�w@��@��@�@n/@;d@&@
=@��@��@u%@�@@��@�o@�#@�~@f�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B�	B�#B�#B�BޞB��B�B�&B�FB�FB��B��B�2B�8B�B�B��B�8B�sB��B��B�B�IB�&B��B��B��BXEB>�B%�B�B�B��B�YB��Bz�Bv�B|jB|�Bw�BshBq�Bo�Bm�BgRBgRBabB_pBY�BX_BWsBR�BQ�BL0BG�BDBC{BA�B@ B<�B:DB6�B2�B&2B�B=B
B�wB�B�[B�*B�:B��B��B��B�B��B��B��B�B}qBz�Bt�BkB\�BNVB9rB�B��B��B�/B�B�9B�)B�6B�B�B��B�B�=B�iBwLBl�Bb�BX_BS[BJ�B<B1[B$�B�BKBJB
��B
�`B
�B
�B
�
B
޸B
��B
ɆB
ðB
��B
��B
��B
cB
m�B
_�B
VB
L�B
H�B
=qB
6FB
$�B
kB
gB
B
jB
)B
�B
�B	�}B	��B	�|B	�}B	�B	�QB	�ZB	��B	�]B	��B	�B	��B	՛B	�FB	�uB	��B	ЗB	ѷB	��B	ЗB	��B	��B	ʌB	ǔB	�3B	��B	�B	��B	��B	��B	�B	�B	�2B	��B	�!B	��B	�AB	�GB	�iB	��B	��B	��B	��B	�!B	��B	��B	��B	��B	��B	��B	�0B	�SB	{�B	p�B	k�B	h�B	gB	e�B	bB	]�B	O�B	DB	D�B	9$B	5ZB	2|B	2�B	($B	&�B	$�B	$ZB	 BB	dB	CB	�B	�B	MB	�B	oB	�B	B	�B	zB	tB	B	B	aB	uB	�B	�B��B�HB�(B��B��B��B�RB��B�FB��B�B��B��B��B��B��B��B��B�wB�]B�B��B�B�B�B�B��B�eB��B�B��B�_B�>B�B�B�8B�B�8B�B�LB�fB��B�B�RB�XB��B�B��B�B��B��B�B��B�B�/B�OB�B��B�B��B��B��B��B��B�PB��B��B	 �B	mB	�B	�B		RB	B	B	�B	�B	NB	�B	B	�B	�B	�B	KB	�B	qB	�B	 �B	"NB	$�B	%�B	&�B	'B	(XB	+�B	,qB	/5B	3hB	5?B	6�B	6�B	8lB	9rB	8�B	9�B	=�B	AoB	D�B	J=B	M6B	P�B	Q�B	TaB	W�B	[#B	^5B	aHB	cB	c�B	eB	e�B	fB	f�B	f�B	g8B	h
B	i*B	iDB	i�B	kB	mwB	oB	qvB	r�B	t�B	xB	x�B	xlB	}"B	�-B	�B	�\B	��B	��B	�B	�vB	�B	�ZB	��B	��B	�KB	�kB	��B	��B	�B	�qB	��B	��B	�oB	�B	�B	�vB	��B	�-B	��B	��B	��B	��B	��B	�lB	�$B	�rB	�*B	��B	�dB	��B	��B	��B	��B	��B	��B	��B	��B	āB	ҽB	��B	�ZB	�B	�RB	�_B	�eB	�B	��B	��B	�B	��B	�B	�B	�cB	��B	��B	��B	��B	�vB	��B	�-B	�-B	��B	�MB	�B	��B	��B	�%B	��B	�`B	�FB	�FB	�2B	��B
�B
HB
�B
�B
�B
�B
�B
�B
�B
�B
!B
�B
 \B
 �B
!-B
!|B
!�B
"B
"�B
#�B
%zB
($B
,�B
0�B
8�B
9�B
;JB
>wB
@iB
C-B
D�B
FB
F�B
G�B
G�B
H�B
IRB
I�B
J=B
J�B
K)B
K�B
L�B
NB
N<B
N�B
QhB
SuB
T�B
W$B
X�B
YeB
Z�B
\�B
^B
_!B
_�B
`�B
c�B
e�B
h�B
kkB
lWB
mB
o B
o�B
p�B
q�B
r-B
q�B
r|B
r�B
sB
s�B
tB
t�B
u�B
v�B
v�B
w�B
x8B
yrB
z�B
|PB
��B
��B
�B
�B
�B
�EB
�XB
��B
�B
��B
� B
��B
�&B
�FB
�2B
��B
��B
�EB
��B
�dB
��B
�|B
��B
��B
�NB
��B
��B
��B
��B
�zB
��B
�2B
�8B
��B
��B
��B
��B
��B
�KB
��B
�B
��B
�vB
�%B
�FB
�zB
�`B
��B
��B
�LB
�fB
��B
��B
��B
��B
��B
�8B
��B
��B
��B
�	B
�>B
��B
��B
�B
��B
�xB
�^B
�B
��B
��B
��B
�BB
��B
�(B
�B
�mB
�9B
ŢB
żB
ŢB
ȴB
�B
�<B
��B
�\B
ϑB
��B
�.B
�}B
��B
�NB
ѝB
ѷB
��B
��B
�B
��B
�uB
ԯB
ՁB
��B
��B
�1B
ٴB
��B
�B
�kB
�#B
�CB
�IB
ݲB
��B
�5B
�jB
ޞB
޸B
��B
��B
��B
�B
�!B
��B
�B
�|B
�|B
�4B
��B
�B
�tB
�zB
�zB
�zB
��B
�fB
�B
�8B
�B
�
B
�$B
�XB
��B
��B
�DB
�yB
�_B
�KB
�B
�]B
�IB
�}B
�B
��B
�[B
�B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�*B
��B
�B
��B
��B
��B
�6B
�jB
��B
��B
�<B
��B
��B
��B
�qB
��B
�(B
��B
��B
��B
�HB
��B
��B  B �BoB�B�BB�B3B�B�B�B?BYB�B+B�BB�B�B�B�B	B
	B
XB
�B
�BB^B^BDB�B�B�BBPBPB�B�BB"B<B<B�B�B�B�BBB(B(B(B�B.B�B�BB BB4B4B4B�B:B�BBB[B�B,BFBaB�B�B�B�BB�B�ByB�B�BKBeB�B�BBQBkB�B�B�B=B�B)BxBIB�BjBOBjB�B�B�B�BBpB�B�B�B BB 'B �B"NB"NB"hB"�B"�B"�B"�B"�B"�B#TB#�B$tB%zB&2B&LB&fB&fB&�B'B'mB'�B'�B'�B'�B'�B'�B(�B(�B)*B)_B)�B)�B*KB+6B,=B,qB,�B,�B,�B,�B-B-CB.IB.IB.cB.�B.�B.�B.�B/ B/5B/5B/�B0!B0oB0oB0�B0�B1B1B1'B1'B1B1B1B1B1'B1[B1�B2�B2�B3B33B33B33B33B3MB3MB3hB3�B3�B3�B4B4B4B4�B4�B4�B5%B5�B5�B5�B6+B6`B6`B6�B6�B6�B6�B72B7LB7�B7�B8lB8�B9>B9�B9�B9�B9�B9�B9�B:B:^B:xB:xB:�B:�B:�B:�B;B;B<�B=B="B=qB=qB=�B=�B=�B=�B=�B>B>B>(B>]B>�B>�B>�B?.B?}B?�B@4B@OB@iB@�B@�B@�B@�B@�BABAUBAUBA�BA�BA�BBuBB�BB�BB�BB�BB�BC{BC�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B��B��B�	B�#B�#B�BޞB��B�B�&B�FB�FB��B��B�2B�8B�B�B��B�8B�sB��B��B�B�IB�&B��B��B��BXEB>�B%�B�B�B��B�YB��Bz�Bv�B|jB|�Bw�BshBq�Bo�Bm�BgRBgRBabB_pBY�BX_BWsBR�BQ�BL0BG�BDBC{BA�B@ B<�B:DB6�B2�B&2B�B=B
B�wB�B�[B�*B�:B��B��B��B�B��B��B��B�B}qBz�Bt�BkB\�BNVB9rB�B��B��B�/B�B�9B�)B�6B�B�B��B�B�=B�iBwLBl�Bb�BX_BS[BJ�B<B1[B$�B�BKBJB
��B
�`B
�B
�B
�
B
޸B
��B
ɆB
ðB
��B
��B
��B
cB
m�B
_�B
VB
L�B
H�B
=qB
6FB
$�B
kB
gB
B
jB
)B
�B
�B	�}B	��B	�|B	�}B	�B	�QB	�ZB	��B	�]B	��B	�B	��B	՛B	�FB	�uB	��B	ЗB	ѷB	��B	ЗB	��B	��B	ʌB	ǔB	�3B	��B	�B	��B	��B	��B	�B	�B	�2B	��B	�!B	��B	�AB	�GB	�iB	��B	��B	��B	��B	�!B	��B	��B	��B	��B	��B	��B	�0B	�SB	{�B	p�B	k�B	h�B	gB	e�B	bB	]�B	O�B	DB	D�B	9$B	5ZB	2|B	2�B	($B	&�B	$�B	$ZB	 BB	dB	CB	�B	�B	MB	�B	oB	�B	B	�B	zB	tB	B	B	aB	uB	�B	�B��B�HB�(B��B��B��B�RB��B�FB��B�B��B��B��B��B��B��B��B�wB�]B�B��B�B�B�B�B��B�eB��B�B��B�_B�>B�B�B�8B�B�8B�B�LB�fB��B�B�RB�XB��B�B��B�B��B��B�B��B�B�/B�OB�B��B�B��B��B��B��B��B�PB��B��B	 �B	mB	�B	�B		RB	B	B	�B	�B	NB	�B	B	�B	�B	�B	KB	�B	qB	�B	 �B	"NB	$�B	%�B	&�B	'B	(XB	+�B	,qB	/5B	3hB	5?B	6�B	6�B	8lB	9rB	8�B	9�B	=�B	AoB	D�B	J=B	M6B	P�B	Q�B	TaB	W�B	[#B	^5B	aHB	cB	c�B	eB	e�B	fB	f�B	f�B	g8B	h
B	i*B	iDB	i�B	kB	mwB	oB	qvB	r�B	t�B	xB	x�B	xlB	}"B	�-B	�B	�\B	��B	��B	�B	�vB	�B	�ZB	��B	��B	�KB	�kB	��B	��B	�B	�qB	��B	��B	�oB	�B	�B	�vB	��B	�-B	��B	��B	��B	��B	��B	�lB	�$B	�rB	�*B	��B	�dB	��B	��B	��B	��B	��B	��B	��B	��B	āB	ҽB	��B	�ZB	�B	�RB	�_B	�eB	�B	��B	��B	�B	��B	�B	�B	�cB	��B	��B	��B	��B	�vB	��B	�-B	�-B	��B	�MB	�B	��B	��B	�%B	��B	�`B	�FB	�FB	�2B	��B
�B
HB
�B
�B
�B
�B
�B
�B
�B
�B
!B
�B
 \B
 �B
!-B
!|B
!�B
"B
"�B
#�B
%zB
($B
,�B
0�B
8�B
9�B
;JB
>wB
@iB
C-B
D�B
FB
F�B
G�B
G�B
H�B
IRB
I�B
J=B
J�B
K)B
K�B
L�B
NB
N<B
N�B
QhB
SuB
T�B
W$B
X�B
YeB
Z�B
\�B
^B
_!B
_�B
`�B
c�B
e�B
h�B
kkB
lWB
mB
o B
o�B
p�B
q�B
r-B
q�B
r|B
r�B
sB
s�B
tB
t�B
u�B
v�B
v�B
w�B
x8B
yrB
z�B
|PB
��B
��B
�B
�B
�B
�EB
�XB
��B
�B
��B
� B
��B
�&B
�FB
�2B
��B
��B
�EB
��B
�dB
��B
�|B
��B
��B
�NB
��B
��B
��B
��B
�zB
��B
�2B
�8B
��B
��B
��B
��B
��B
�KB
��B
�B
��B
�vB
�%B
�FB
�zB
�`B
��B
��B
�LB
�fB
��B
��B
��B
��B
��B
�8B
��B
��B
��B
�	B
�>B
��B
��B
�B
��B
�xB
�^B
�B
��B
��B
��B
�BB
��B
�(B
�B
�mB
�9B
ŢB
żB
ŢB
ȴB
�B
�<B
��B
�\B
ϑB
��B
�.B
�}B
��B
�NB
ѝB
ѷB
��B
��B
�B
��B
�uB
ԯB
ՁB
��B
��B
�1B
ٴB
��B
�B
�kB
�#B
�CB
�IB
ݲB
��B
�5B
�jB
ޞB
޸B
��B
��B
��B
�B
�!B
��B
�B
�|B
�|B
�4B
��B
�B
�tB
�zB
�zB
�zB
��B
�fB
�B
�8B
�B
�
B
�$B
�XB
��B
��B
�DB
�yB
�_B
�KB
�B
�]B
�IB
�}B
�B
��B
�[B
�B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�*B
��B
�B
��B
��B
��B
�6B
�jB
��B
��B
�<B
��B
��B
��B
�qB
��B
�(B
��B
��B
��B
�HB
��B
��B  B �BoB�B�BB�B3B�B�B�B?BYB�B+B�BB�B�B�B�B	B
	B
XB
�B
�BB^B^BDB�B�B�BBPBPB�B�BB"B<B<B�B�B�B�BBB(B(B(B�B.B�B�BB BB4B4B4B�B:B�BBB[B�B,BFBaB�B�B�B�BB�B�ByB�B�BKBeB�B�BBQBkB�B�B�B=B�B)BxBIB�BjBOBjB�B�B�B�BBpB�B�B�B BB 'B �B"NB"NB"hB"�B"�B"�B"�B"�B"�B#TB#�B$tB%zB&2B&LB&fB&fB&�B'B'mB'�B'�B'�B'�B'�B'�B(�B(�B)*B)_B)�B)�B*KB+6B,=B,qB,�B,�B,�B,�B-B-CB.IB.IB.cB.�B.�B.�B.�B/ B/5B/5B/�B0!B0oB0oB0�B0�B1B1B1'B1'B1B1B1B1B1'B1[B1�B2�B2�B3B33B33B33B33B3MB3MB3hB3�B3�B3�B4B4B4B4�B4�B4�B5%B5�B5�B5�B6+B6`B6`B6�B6�B6�B6�B72B7LB7�B7�B8lB8�B9>B9�B9�B9�B9�B9�B9�B:B:^B:xB:xB:�B:�B:�B:�B;B;B<�B=B="B=qB=qB=�B=�B=�B=�B=�B>B>B>(B>]B>�B>�B>�B?.B?}B?�B@4B@OB@iB@�B@�B@�B@�B@�BABAUBAUBA�BA�BA�BBuBB�BB�BB�BB�BB�BC{BC�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221015034048  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221015034053  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221015034054  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221015034054                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221015124058  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221015124058  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20221015035843                      G�O�G�O�G�O�                
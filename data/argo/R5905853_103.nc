CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:41:12Z creation;2022-06-04T17:41:12Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174112  20220610131508  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               gA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @٧"����1   @٧"��G�@/mV��c]\(�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  BBffBG33BO��BX  B`ffBfffBp  BxffB��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C�C33C�fC  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DJ��DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @1�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A�
=B�B�B�B�B'�B/�B7�BBQ�BG�BO�BW�B`Q�BfQ�Bo�BxQ�B�B���B���B���B���B���B���B���B���B���B�(�B�\)B�\)B���B�B���B���B���B���B���B���B���B���B���B���B�(�B�B���B���B���B���B���B���C��C��C�GC��C	��C��C��C��C{C.C�GC��C��C��C��C��C!��C#��C%��C'��C)�GC+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CH{CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ�RDK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS�DTDT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��DdxRDd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�DsDs~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D���D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D悏D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�l)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��9A�͟A�ʌA�ɆA�ɆA���A���A��A�ΥA���A���A���A��mA���A�՛A�՛A�خA��A��2A�҉A��TA���A���A��zAԽqAԤ�AԥzAԙeAҦLAɣ�A�3�A�d�A�x8A���A�^jA���A��A���A���A���A���A�A��MA��"A���A�zA�9XA��iA��jA���A�
rA�>�A�W?A���A��@A���A�_A��OA�1[A���A��SA���A��A�#A�RTA���A�;dA�gmA���A�)�A�A��A�p�A|��As�An��Aj�Ah9�Af�<Ad9�Aa�'A`|�A_��A^��A\1ATh�AO�AL_AI��AG��AE�"AC��AA��A@�A?`�A=FA;�A;�A:��A9�NA9M�A9�A8�A8�A8�<A8��A8A�A7�:A6��A6qA6�A5�A5T�A5%A4�@A4,=A3[�A2dZA0�2A0A.��A.7�A-��A,�sA+�5A)�UA'��A&�>A&+kA%خA%�fA%xlA$��A$|�A$tTA%�A$ߤA%9�A%4�A$�A$�QA$��A$uA#\�A"ɆA"xA",=A"A!�9A!}VA!B[A!1A *�A�A�A��A��A�oAdZA��A!AĜA�mA�.AS�A#:A��AjA1�A��A	A�Ax�AS&AJA��A�~At�A�A��A�AP�A��AE9A��A�+A%A�A{A%�A&�A�A��A$AȴA�XA�A��AC�A
�A
�A
w�A
jA
FA
�A	�MA	��A	�RA	��A	�A	c�A	1�A��A�AffA�:A�:A�Aj�AMA�A��A�gA��A��A�A��A �,A ��A �A�AeA �A �SA c�A .�@��g@�\�@��@�Dg@� i@���@�9X@�,�@�Xy@�$t@���@���@�s@�@�~�@��@��,@��W@�%F@�.I@�8�@궮@��@�@��K@��@�&@�L@�r@�@�@琗@�<�@���@�9X@��@�S@�s�@�a�@�A @�Y@�+@�/�@�S@���@ಖ@�a@�@ݡ�@ݫ�@�q@�ff@ۡ�@�+@��?@�m�@�ݘ@�Y�@أ�@��A@�x�@�\)@�Y@֕�@�GE@���@�t�@���@ԧ@��@ќ@�=�@Ѕ�@�p;@ϲ-@���@�g8@��@�H�@̣�@�@�O�@��@���@��@���@�H�@�j@�+�@�Ft@Ů�@Ō~@�s@�@�l�@�	�@��3@�W?@�q�@�#:@�@��@@�)_@���@���@��@��$@�F�@��f@�Z@���@��f@�a@��@�l�@�8�@�S�@�+@��@��h@�	�@�?}@�ȴ@���@�|�@�8�@��H@�o�@���@�^5@��@��&@��P@�+@���@�oi@���@�!-@��\@��@�=@���@��&@���@�^�@��@�֡@���@�r�@�e@��	@�\�@��"@���@�h�@�3�@��}@�]�@��j@���@�|�@�D�@���@���@�X�@��2@���@���@�-@���@���@��5@�w�@��@��
@�qv@��@��'@�tT@��.@��@@���@�X�@�#�@��@��)@�W�@� �@��3@��V@�;d@��]@���@��@�o @��'@�^5@�-�@� �@���@�w2@�S&@�S@��z@�`�@�B[@�*�@��F@�\�@�-w@���@�V�@���@��@�N<@� \@��@�A�@��@��m@�~�@�X�@��v@�Q�@��z@���@�T�@��@��@���@���@���@��U@��4@�~(@�m�@�C�@�˒@��=@�{J@�W?@�1�@��f@��1@�_�@�@�@�	@�خ@��7@��M@�z@�A�@���@��@��"@�}�@�b�@�5�@���@�ߤ@�ȴ@���@�J@��@���@�\)@�=�@�.I@�-w@�,�@�(�@�V@��H@�҉@���@� �@��@���@�ԕ@���@�U�@�L�@�%@���@�v�@�h
@�a|@�Ta@�:*@��>@���@�\�@�K�@�0�@�(@���@��@���@�GE@��Z@��t@��@��f@�b�@�>�@� \@��M@��)@��L@���@��@�9X@�@��@@��S@���@�[W@�9�@��@���@��@��
@��3@���@�qv@�S&@�)_@��@���@�V�@�:�@�.�@�+k@�{@��@��[@�v`@�0�@���@��@��_@�q@�C-@�{@�&@��@�@P�@Y@~�8@~��@}�@}#�@|tT@{j�@z��@y��@y(�@x�@xS�@w� @wF�@v��@vB[@v#:@u��@u<6@t�K@tI�@t9X@sݘ@s.I@r�c@r��@r��@ri�@rQ@rB[@q��@q \@pɆ@p7�@o�@n��@n��@n!�@m�@m�@mo @l�/@lC-@k��@k��@j�@i�@iF@h�5@hu�@h2�@g�
@gY@fxl@e��@dѷ@d�.@c��@cO@b�@b�6@bYK@b	@a��@ao @aDg@`��@_�]@_|�@_(@^�y@^��@^�h@^��@^Ta@^-@^�@^ �@]�@\�@\@[��@[�@Z�,@Z��@ZZ�@Y�)@Y�^@Y��@Y�h@Y2a@X�@XG@W�:@WS�@V�M@V�@Vff@VW�@VB[@V#:@V	@Vu@U�C@UVm@T��@T��@Th�@T/�@T�@S��@S�}@S��@S�:@S��@S�4@SA�@S@R��@Q��@Q^�@Q�@P��@Pg8@PS�@O��@O_p@N��@N+k@M��@MT�@MO�@MB�@Mq@L�5@L��@LFt@L-�@L"h@L1@K�@K�@K��@K�k@KX�@J��@J~�@I�>@I�=@Ihs@I	l@H�@H�u@H  @GU�@F�+@FV@F-@Fu@E��@E0�@D��@C�@C��@Cy�@Bں@A��@A��@A\�@A&�@@��@@��@@��@@Ft@?��@?A�@>i�@=��@=��@=:�@<�?@<�@<D�@;��@;�4@; i@:~�@:&�@9�o@9Q�@9�@8��@8PH@8�@7ƨ@7t�@7Z�@6�8@6�x@5��@5c@5A @54@54@5�@4��@4��@4��@3��@3a@3,�@2��@2�@2��@1��@1�N@1�H@1�7@1<6@0�K@0�z@09X@/�@/�*@/Y@.R�@.5?@-�@-�3@-��@-F@,�p@,Xy@+��@+�a@+�*@+�V@+��@+�4@+iD@*�H@*�\@*?@*@)��@)�@(ی@(�@(Q�@(2�@'�Q@'��@'��@'�V@'�f@'n/@'&@&��@&��@&�6@&�F@&~�@&W�@&1�@%�^@%��@%-w@$�@$�`@$�)@$�u@$��@$�D@$w�@$`�@$>B@$7@#� @#|�@#@"��@"�@"�X@"�b@"�x@"��@"\�@"�@!�o@!�@!F@!@@ ��@ z�@ 1'@�@�P@_p@,�@�@�h@�A@q�@�@��@�@��@�~@j@S&@�@��@U2@x@�K@��@x@X�@/�@
=@��@�X@W�@�@�#@�C@w2@N<@IR@#�@��@��@�9@�@��@�	@8@�X@��@^5@0U@��@��@p�@:�@�@��@`�@I�@�@�w@�{@S�@�@�s@�<@��@d�@$�@��@�C@(�@��@��@]d@:�@7@�@��@�@��@s@o�@X�@ i@��@Q@;�@�@_@�)@�d@�"@f�@%@�@r�@7�@��@�f@K�@)_@
��@
�h@
��@
}V@
8�@
	@	�o@	��@	��@	e,@	�@��@��@��@��@��@Xy@>B@�@�@��@l�@W?@Mj@&@�8@�@�@ߤ@�'@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��9A�͟A�ʌA�ɆA�ɆA���A���A��A�ΥA���A���A���A��mA���A�՛A�՛A�خA��A��2A�҉A��TA���A���A��zAԽqAԤ�AԥzAԙeAҦLAɣ�A�3�A�d�A�x8A���A�^jA���A��A���A���A���A���A�A��MA��"A���A�zA�9XA��iA��jA���A�
rA�>�A�W?A���A��@A���A�_A��OA�1[A���A��SA���A��A�#A�RTA���A�;dA�gmA���A�)�A�A��A�p�A|��As�An��Aj�Ah9�Af�<Ad9�Aa�'A`|�A_��A^��A\1ATh�AO�AL_AI��AG��AE�"AC��AA��A@�A?`�A=FA;�A;�A:��A9�NA9M�A9�A8�A8�A8�<A8��A8A�A7�:A6��A6qA6�A5�A5T�A5%A4�@A4,=A3[�A2dZA0�2A0A.��A.7�A-��A,�sA+�5A)�UA'��A&�>A&+kA%خA%�fA%xlA$��A$|�A$tTA%�A$ߤA%9�A%4�A$�A$�QA$��A$uA#\�A"ɆA"xA",=A"A!�9A!}VA!B[A!1A *�A�A�A��A��A�oAdZA��A!AĜA�mA�.AS�A#:A��AjA1�A��A	A�Ax�AS&AJA��A�~At�A�A��A�AP�A��AE9A��A�+A%A�A{A%�A&�A�A��A$AȴA�XA�A��AC�A
�A
�A
w�A
jA
FA
�A	�MA	��A	�RA	��A	�A	c�A	1�A��A�AffA�:A�:A�Aj�AMA�A��A�gA��A��A�A��A �,A ��A �A�AeA �A �SA c�A .�@��g@�\�@��@�Dg@� i@���@�9X@�,�@�Xy@�$t@���@���@�s@�@�~�@��@��,@��W@�%F@�.I@�8�@궮@��@�@��K@��@�&@�L@�r@�@�@琗@�<�@���@�9X@��@�S@�s�@�a�@�A @�Y@�+@�/�@�S@���@ಖ@�a@�@ݡ�@ݫ�@�q@�ff@ۡ�@�+@��?@�m�@�ݘ@�Y�@أ�@��A@�x�@�\)@�Y@֕�@�GE@���@�t�@���@ԧ@��@ќ@�=�@Ѕ�@�p;@ϲ-@���@�g8@��@�H�@̣�@�@�O�@��@���@��@���@�H�@�j@�+�@�Ft@Ů�@Ō~@�s@�@�l�@�	�@��3@�W?@�q�@�#:@�@��@@�)_@���@���@��@��$@�F�@��f@�Z@���@��f@�a@��@�l�@�8�@�S�@�+@��@��h@�	�@�?}@�ȴ@���@�|�@�8�@��H@�o�@���@�^5@��@��&@��P@�+@���@�oi@���@�!-@��\@��@�=@���@��&@���@�^�@��@�֡@���@�r�@�e@��	@�\�@��"@���@�h�@�3�@��}@�]�@��j@���@�|�@�D�@���@���@�X�@��2@���@���@�-@���@���@��5@�w�@��@��
@�qv@��@��'@�tT@��.@��@@���@�X�@�#�@��@��)@�W�@� �@��3@��V@�;d@��]@���@��@�o @��'@�^5@�-�@� �@���@�w2@�S&@�S@��z@�`�@�B[@�*�@��F@�\�@�-w@���@�V�@���@��@�N<@� \@��@�A�@��@��m@�~�@�X�@��v@�Q�@��z@���@�T�@��@��@���@���@���@��U@��4@�~(@�m�@�C�@�˒@��=@�{J@�W?@�1�@��f@��1@�_�@�@�@�	@�خ@��7@��M@�z@�A�@���@��@��"@�}�@�b�@�5�@���@�ߤ@�ȴ@���@�J@��@���@�\)@�=�@�.I@�-w@�,�@�(�@�V@��H@�҉@���@� �@��@���@�ԕ@���@�U�@�L�@�%@���@�v�@�h
@�a|@�Ta@�:*@��>@���@�\�@�K�@�0�@�(@���@��@���@�GE@��Z@��t@��@��f@�b�@�>�@� \@��M@��)@��L@���@��@�9X@�@��@@��S@���@�[W@�9�@��@���@��@��
@��3@���@�qv@�S&@�)_@��@���@�V�@�:�@�.�@�+k@�{@��@��[@�v`@�0�@���@��@��_@�q@�C-@�{@�&@��@�@P�@Y@~�8@~��@}�@}#�@|tT@{j�@z��@y��@y(�@x�@xS�@w� @wF�@v��@vB[@v#:@u��@u<6@t�K@tI�@t9X@sݘ@s.I@r�c@r��@r��@ri�@rQ@rB[@q��@q \@pɆ@p7�@o�@n��@n��@n!�@m�@m�@mo @l�/@lC-@k��@k��@j�@i�@iF@h�5@hu�@h2�@g�
@gY@fxl@e��@dѷ@d�.@c��@cO@b�@b�6@bYK@b	@a��@ao @aDg@`��@_�]@_|�@_(@^�y@^��@^�h@^��@^Ta@^-@^�@^ �@]�@\�@\@[��@[�@Z�,@Z��@ZZ�@Y�)@Y�^@Y��@Y�h@Y2a@X�@XG@W�:@WS�@V�M@V�@Vff@VW�@VB[@V#:@V	@Vu@U�C@UVm@T��@T��@Th�@T/�@T�@S��@S�}@S��@S�:@S��@S�4@SA�@S@R��@Q��@Q^�@Q�@P��@Pg8@PS�@O��@O_p@N��@N+k@M��@MT�@MO�@MB�@Mq@L�5@L��@LFt@L-�@L"h@L1@K�@K�@K��@K�k@KX�@J��@J~�@I�>@I�=@Ihs@I	l@H�@H�u@H  @GU�@F�+@FV@F-@Fu@E��@E0�@D��@C�@C��@Cy�@Bں@A��@A��@A\�@A&�@@��@@��@@��@@Ft@?��@?A�@>i�@=��@=��@=:�@<�?@<�@<D�@;��@;�4@; i@:~�@:&�@9�o@9Q�@9�@8��@8PH@8�@7ƨ@7t�@7Z�@6�8@6�x@5��@5c@5A @54@54@5�@4��@4��@4��@3��@3a@3,�@2��@2�@2��@1��@1�N@1�H@1�7@1<6@0�K@0�z@09X@/�@/�*@/Y@.R�@.5?@-�@-�3@-��@-F@,�p@,Xy@+��@+�a@+�*@+�V@+��@+�4@+iD@*�H@*�\@*?@*@)��@)�@(ی@(�@(Q�@(2�@'�Q@'��@'��@'�V@'�f@'n/@'&@&��@&��@&�6@&�F@&~�@&W�@&1�@%�^@%��@%-w@$�@$�`@$�)@$�u@$��@$�D@$w�@$`�@$>B@$7@#� @#|�@#@"��@"�@"�X@"�b@"�x@"��@"\�@"�@!�o@!�@!F@!@@ ��@ z�@ 1'@�@�P@_p@,�@�@�h@�A@q�@�@��@�@��@�~@j@S&@�@��@U2@x@�K@��@x@X�@/�@
=@��@�X@W�@�@�#@�C@w2@N<@IR@#�@��@��@�9@�@��@�	@8@�X@��@^5@0U@��@��@p�@:�@�@��@`�@I�@�@�w@�{@S�@�@�s@�<@��@d�@$�@��@�C@(�@��@��@]d@:�@7@�@��@�@��@s@o�@X�@ i@��@Q@;�@�@_@�)@�d@�"@f�@%@�@r�@7�@��@�f@K�@)_@
��@
�h@
��@
}V@
8�@
	@	�o@	��@	��@	e,@	�@��@��@��@��@��@Xy@>B@�@�@��@l�@W?@Mj@&@�8@�@�@ߤ@�'@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B�5B�5B�OB�iB�iB�iB�OB�iB�OB�iB��B��B��B��B�;B��B�'B�B��B��B��B�GB�LB��B�.B�HB��B	"B
AUB
��B
��B
��B
�LB
��BB�BFB�B4�BPHBR�BX�BV�BZ7BUBS�BU2BT�BU�BU�BW$BS�BO�BM6B8�B)BfB�B
�	B
�B
�EB
�B
��B
��B
xB
YB
9rB
-)B
B
�B
B	�LB	�SB	��B	�SB	�B	�4B	z�B	sMB	i_B	d�B	a-B	\�B	Q4B	>�B	,�B	!bB	7B	'B	1�B	8B	4�B	4B	5�B	G_B	T�B	^5B	b�B	h�B	m�B	p�B	r|B	s�B	t9B	t�B	y>B	��B	��B	��B	��B	��B	��B	�MB	�B	�B	�4B	��B	�
B	�RB	�fB	�FB	�ZB	�B	�xB	�B	�BB	��B	��B	��B	��B	�}B	��B	��B	�B	�:B	�+B	�HB	�B	��B	��B	��B	��B	�LB	��B	�$B	�DB	�B	�B	�(B	��B	��B
  B	��B	��B	�PB
gB
�B
PB
�B
B
B
�B
B
�B
�B	��B	�B	��B	��B	�}B
uB
-B
�B
�B
�B
[B
�B
oB
�B
 �B
B	�PB	�$B	��B	�0B	�.B	��B
 �B
�B
SB
B
�B
�B
%B
  B	��B	��B	�6B	��B
B
 B
UB
'B
{B
�B
�B
%B
%B
%B
YB
�B
zB
�B
EB
YB
mB
[B
 �B
�B
{B
aB
 OB	�B
UB
�B
�B
�B
MB
�B
@B
�B
2B

B
mB
�B
�B
mB
�B
NB
�B
�B
{B
}B
�B
%B
 �B	�wB	�B	�_B	�B	�LB	�B	׍B	�B	�/B	�\B	�!B	�B	�0B	�>B	�sB	�sB	�B	�*B	�B	�$B	�B	�B	� B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�jB	��B	�dB	�~B	�xB	��B	ݘB	��B	��B	��B	�CB	��B	ݘB	�IB	�B	��B	�'B	�'B	��B	�B	�B	��B	��B	�B	�-B	�B	�	B	��B	��B	ܬB	�B	�IB	��B	�dB	�dB	�OB	��B	�!B	�;B	ߊB	��B	��B	�B	�bB	�|B	�B	�B	�fB	�LB	�B	��B	�8B	�$B	�_B	�QB	�kB	�B	��B	�B	�wB	�OB	�B	��B	��B	��B	�B	�3B	�B	�2B	�XB	�xB	�PB	��B	�qB	��B	�qB	��B	�.B	�.B	��B	��B	��B	�B	�.B	��B	��B	��B	��B	�cB	�}B	�.B	��B	��B	�BB	�B	��B	�qB	�PB	�B	�dB	�0B	��B	��B	��B	�6B	�"B	�"B	��B	��B
 B
�B
[B
�B
�B
�B
;B
B
 �B
 �B
 iB
 iB
 �B
 �B
 �B
 iB
 B
 OB
  B	�}B	�.B	��B	�B	�.B	�HB	��B	��B
  B
  B
 iB
 �B
 �B
 B
;B
�B
B
AB
AB
�B
-B
�B
�B
�B
�B
�B
B
%B
�B
�B
�B
B
+B
+B
�B
�B
�B
�B
1B
KB
	B
�B
	B
	�B
�B
B
�B
�B
�B
VB
<B
�B
(B
BB
\B
B
�B
�B
uB
�B
�B
B
B
1B
�B
kB
7B
kB
�B
B
�B
�B
/B
dB
�B
�B
�B
�B
B
�B
�B
B
OB
VB
 \B
 �B
 �B
!-B
!HB
!|B
!HB
 �B
!B
!|B
"B
"4B
"NB
"hB
"�B
#B
#:B
# B
#:B
#:B
#B
#:B
#B
#:B
#nB
#TB
#�B
$@B
$�B
$tB
$�B
$�B
$�B
%FB
%�B
&LB
&LB
&fB
&�B
&�B
&�B
'mB
'�B
(�B
(�B
(�B
(�B
)B
)*B
)*B
)_B
)yB
)yB
)yB
)DB
)�B
*�B
+B
+QB
+6B
,"B
,WB
,�B
-)B
.cB
.IB
.�B
.�B
/5B
/OB
/iB
/�B
/�B
0!B
0B
/�B
/�B
0;B
0!B
0UB
0oB
0�B
1'B
1vB
1�B
2GB
2�B
3B
3�B
3�B
3�B
3�B
49B
4�B
4�B
5ZB
4�B
5�B
6+B
5�B
5�B
4�B
4�B
4�B
5B
5tB
5ZB
5tB
5?B
5�B
6+B
6zB
6�B
6�B
6�B
7�B
7LB
7LB
7fB
72B
72B
7B
7B
7�B
7fB
8B
9	B
9$B
9$B
9XB
9>B
9rB
9rB
9�B
:B
:xB
;�B
;B
:�B
;0B
;B
<�B
=B
="B
=�B
=�B
>(B
?�B
?�B
?�B
@iB
@�B
@�B
A B
AoB
A�B
BB
B'B
B�B
C�B
DgB
D�B
D�B
EB
EB
E9B
ESB
EmB
EmB
EmB
E�B
FtB
F�B
G+B
G_B
G�B
G�B
G�B
H1B
H1B
H1B
HKB
H�B
H�B
IRB
I�B
I�B
J#B
J=B
JrB
JXB
JrB
J�B
J�B
J�B
J�B
J�B
K)B
KxB
K�B
K�B
K�B
K�B
LB
L0B
L0B
L0B
L0B
LJB
L~B
L�B
MjB
M�B
NB
N�B
N�B
NpB
N�B
O(B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
QNB
QNB
QhB
QhB
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
RoB
R�B
SB
R�B
S@B
S&B
S@B
S�B
T,B
T�B
T�B
T�B
T�B
UMB
U�B
U�B
V�B
V�B
V�B
W$B
W�B
W�B
W�B
W�B
W�B
W�B
X+B
XEB
X�B
X�B
Y�B
ZB
Z7B
Z7B
Z�B
Z�B
Z�B
Z�B
[	B
[qB
[�B
[�B
[�B
\]B
\]B
\�B
\�B
]B
]B
]IB
]/B
]~B
]�B
^B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_�B
_�B
_�B
_�B
_�B
`BB
`BB
`BB
`vB
`�B
`�B
`�B
a-B
a-B
aHB
a�B
bB
a�B
b4B
bhB
bhB
b�B
c B
c�B
c�B
c�B
dB
dB
d&B
dB
d&B
d�B
d�B
eB
e,B
e�B
e�B
fB
fLB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g8B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
iDB
iyB
i�B
jB
jB
j0B
j�B
j�B
kB
j�B
kB
kQB
k�B
lWB
l=B
l=B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
mCB
mCB
m�B
m�B
n�B
n�B
o B
oB
o5B
oiB
o�B
o�B
o�B
o�B
p;B
pUB
p;B
poB
p�B
p�B
p�B
q'B
qvB
q�B
r-B
raB
r�B
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
tB
s�B
s�B
s�B
tB
t9B
tnB
t�B
u%B
u�B
v+B
utB
uZB
u�B
u�B
u�B
vB
v+B
vFB
vzB
v�B
wB
w2B
wLB
w�B
w�B
w�B
xRB
x�B
x�B
x�B
x�B
y	B
y$B
yrB
y�B
y�B
zxB
z�B
z�B
z�B
z�B
{B
{0B
{dB
{B
{B
{B
{�B
|�B
|�B
|�B
}"B
}<B
}qB
}�B
~B
~B
~�B
~�B
.B
.B
�B
�B
� B
�OB
��B
��B
�B
�UB
��B
��B
��B
�'B
�[B
��B
��B
��B
�-B
�-B
�-B
�GB
�aB
��B
��B
��B
��B
�gB
��B
��B
��B
�B
�B
�B
�B
�mB
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B�5B�5B�OB�iB�iB�iB�OB�iB�OB�iB��B��B��B��B�;B��B�'B�B��B��B��B�GB�LB��B�.B�HB��B	"B
AUB
��B
��B
��B
�LB
��BB�BFB�B4�BPHBR�BX�BV�BZ7BUBS�BU2BT�BU�BU�BW$BS�BO�BM6B8�B)BfB�B
�	B
�B
�EB
�B
��B
��B
xB
YB
9rB
-)B
B
�B
B	�LB	�SB	��B	�SB	�B	�4B	z�B	sMB	i_B	d�B	a-B	\�B	Q4B	>�B	,�B	!bB	7B	'B	1�B	8B	4�B	4B	5�B	G_B	T�B	^5B	b�B	h�B	m�B	p�B	r|B	s�B	t9B	t�B	y>B	��B	��B	��B	��B	��B	��B	�MB	�B	�B	�4B	��B	�
B	�RB	�fB	�FB	�ZB	�B	�xB	�B	�BB	��B	��B	��B	��B	�}B	��B	��B	�B	�:B	�+B	�HB	�B	��B	��B	��B	��B	�LB	��B	�$B	�DB	�B	�B	�(B	��B	��B
  B	��B	��B	�PB
gB
�B
PB
�B
B
B
�B
B
�B
�B	��B	�B	��B	��B	�}B
uB
-B
�B
�B
�B
[B
�B
oB
�B
 �B
B	�PB	�$B	��B	�0B	�.B	��B
 �B
�B
SB
B
�B
�B
%B
  B	��B	��B	�6B	��B
B
 B
UB
'B
{B
�B
�B
%B
%B
%B
YB
�B
zB
�B
EB
YB
mB
[B
 �B
�B
{B
aB
 OB	�B
UB
�B
�B
�B
MB
�B
@B
�B
2B

B
mB
�B
�B
mB
�B
NB
�B
�B
{B
}B
�B
%B
 �B	�wB	�B	�_B	�B	�LB	�B	׍B	�B	�/B	�\B	�!B	�B	�0B	�>B	�sB	�sB	�B	�*B	�B	�$B	�B	�B	� B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�jB	��B	�dB	�~B	�xB	��B	ݘB	��B	��B	��B	�CB	��B	ݘB	�IB	�B	��B	�'B	�'B	��B	�B	�B	��B	��B	�B	�-B	�B	�	B	��B	��B	ܬB	�B	�IB	��B	�dB	�dB	�OB	��B	�!B	�;B	ߊB	��B	��B	�B	�bB	�|B	�B	�B	�fB	�LB	�B	��B	�8B	�$B	�_B	�QB	�kB	�B	��B	�B	�wB	�OB	�B	��B	��B	��B	�B	�3B	�B	�2B	�XB	�xB	�PB	��B	�qB	��B	�qB	��B	�.B	�.B	��B	��B	��B	�B	�.B	��B	��B	��B	��B	�cB	�}B	�.B	��B	��B	�BB	�B	��B	�qB	�PB	�B	�dB	�0B	��B	��B	��B	�6B	�"B	�"B	��B	��B
 B
�B
[B
�B
�B
�B
;B
B
 �B
 �B
 iB
 iB
 �B
 �B
 �B
 iB
 B
 OB
  B	�}B	�.B	��B	�B	�.B	�HB	��B	��B
  B
  B
 iB
 �B
 �B
 B
;B
�B
B
AB
AB
�B
-B
�B
�B
�B
�B
�B
B
%B
�B
�B
�B
B
+B
+B
�B
�B
�B
�B
1B
KB
	B
�B
	B
	�B
�B
B
�B
�B
�B
VB
<B
�B
(B
BB
\B
B
�B
�B
uB
�B
�B
B
B
1B
�B
kB
7B
kB
�B
B
�B
�B
/B
dB
�B
�B
�B
�B
B
�B
�B
B
OB
VB
 \B
 �B
 �B
!-B
!HB
!|B
!HB
 �B
!B
!|B
"B
"4B
"NB
"hB
"�B
#B
#:B
# B
#:B
#:B
#B
#:B
#B
#:B
#nB
#TB
#�B
$@B
$�B
$tB
$�B
$�B
$�B
%FB
%�B
&LB
&LB
&fB
&�B
&�B
&�B
'mB
'�B
(�B
(�B
(�B
(�B
)B
)*B
)*B
)_B
)yB
)yB
)yB
)DB
)�B
*�B
+B
+QB
+6B
,"B
,WB
,�B
-)B
.cB
.IB
.�B
.�B
/5B
/OB
/iB
/�B
/�B
0!B
0B
/�B
/�B
0;B
0!B
0UB
0oB
0�B
1'B
1vB
1�B
2GB
2�B
3B
3�B
3�B
3�B
3�B
49B
4�B
4�B
5ZB
4�B
5�B
6+B
5�B
5�B
4�B
4�B
4�B
5B
5tB
5ZB
5tB
5?B
5�B
6+B
6zB
6�B
6�B
6�B
7�B
7LB
7LB
7fB
72B
72B
7B
7B
7�B
7fB
8B
9	B
9$B
9$B
9XB
9>B
9rB
9rB
9�B
:B
:xB
;�B
;B
:�B
;0B
;B
<�B
=B
="B
=�B
=�B
>(B
?�B
?�B
?�B
@iB
@�B
@�B
A B
AoB
A�B
BB
B'B
B�B
C�B
DgB
D�B
D�B
EB
EB
E9B
ESB
EmB
EmB
EmB
E�B
FtB
F�B
G+B
G_B
G�B
G�B
G�B
H1B
H1B
H1B
HKB
H�B
H�B
IRB
I�B
I�B
J#B
J=B
JrB
JXB
JrB
J�B
J�B
J�B
J�B
J�B
K)B
KxB
K�B
K�B
K�B
K�B
LB
L0B
L0B
L0B
L0B
LJB
L~B
L�B
MjB
M�B
NB
N�B
N�B
NpB
N�B
O(B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
QNB
QNB
QhB
QhB
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
RoB
R�B
SB
R�B
S@B
S&B
S@B
S�B
T,B
T�B
T�B
T�B
T�B
UMB
U�B
U�B
V�B
V�B
V�B
W$B
W�B
W�B
W�B
W�B
W�B
W�B
X+B
XEB
X�B
X�B
Y�B
ZB
Z7B
Z7B
Z�B
Z�B
Z�B
Z�B
[	B
[qB
[�B
[�B
[�B
\]B
\]B
\�B
\�B
]B
]B
]IB
]/B
]~B
]�B
^B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_�B
_�B
_�B
_�B
_�B
`BB
`BB
`BB
`vB
`�B
`�B
`�B
a-B
a-B
aHB
a�B
bB
a�B
b4B
bhB
bhB
b�B
c B
c�B
c�B
c�B
dB
dB
d&B
dB
d&B
d�B
d�B
eB
e,B
e�B
e�B
fB
fLB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g8B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
iDB
iyB
i�B
jB
jB
j0B
j�B
j�B
kB
j�B
kB
kQB
k�B
lWB
l=B
l=B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
mCB
mCB
m�B
m�B
n�B
n�B
o B
oB
o5B
oiB
o�B
o�B
o�B
o�B
p;B
pUB
p;B
poB
p�B
p�B
p�B
q'B
qvB
q�B
r-B
raB
r�B
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
tB
s�B
s�B
s�B
tB
t9B
tnB
t�B
u%B
u�B
v+B
utB
uZB
u�B
u�B
u�B
vB
v+B
vFB
vzB
v�B
wB
w2B
wLB
w�B
w�B
w�B
xRB
x�B
x�B
x�B
x�B
y	B
y$B
yrB
y�B
y�B
zxB
z�B
z�B
z�B
z�B
{B
{0B
{dB
{B
{B
{B
{�B
|�B
|�B
|�B
}"B
}<B
}qB
}�B
~B
~B
~�B
~�B
.B
.B
�B
�B
� B
�OB
��B
��B
�B
�UB
��B
��B
��B
�'B
�[B
��B
��B
��B
�-B
�-B
�-B
�GB
�aB
��B
��B
��B
��B
�gB
��B
��B
��B
�B
�B
�B
�B
�mB
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104926  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174112  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174112  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174112                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024119  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024119  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131508                      G�O�G�O�G�O�                
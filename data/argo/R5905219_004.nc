CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2018-06-25T08:15:45Z creation;2018-06-25T08:15:46Z conversion to V3.1      
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
resolution        =���   axis      Z        8  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  D�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  G�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  R�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     8  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     8  c�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     8  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     8  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     8  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     8  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20180625081545  20180625091527  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            7906                            051216                          846 @�i���d�1   @�i���I�@3\(��et�j~�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.�C0�C2  C4  C6  C7�fC9�fC<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D��Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DQ��DRy�DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~fD~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�\)@�\)A�AAG�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�BpQ�BxQ�B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C,{C.{C0{C1��C3��C5��C7�GC9�GC;��C=��C?�GCA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Cb{Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��DxRD�RDxRD��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D$D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DGxRDG��DH~�DH��DI~�DI��DJ�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQxRDQ�RDRxRDR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�DdDd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh�RDi~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�DoDo~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D~D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��)D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D�D��\D�?\D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�G�A�"�A�{A�1A���A���A��A��A��A��A��A��mA��TA��;A��A���AҴ9A҅A�A�A���A�p�A��AϺ^Aϡ�Aϟ�Aϡ�Aϗ�AρA�hsA�ZA�A�A�33A�$�A��A�  A���A�t�A��TA�v�A�A�A̸RA� �A�JA�K�A��AɬA�t�A�O�A�33A�
=A�^5Aǡ�A�Q�A��A��AƼjA�{A�E�A�7LA�33A�A��A�jA��
A�5?A���A�v�A��A��jA���A���A��jA���A�l�A��A�JA�A���A��mA��A�1A���A���A�jA�(�A��RA�M�A��TA���A�^5A�JA���A��hA�33A�  A��hA�bNA�A�A�Q�A��A��;A�dZA�^5A��#A�G�A�(�A��`A�K�A���A�9XA�E�A���A��A� �A�/A�t�A���A�JA���A��wA��FA���A�-A��A��uA�5?A~I�A{�Au�Ap{Aj�DAd5?A`A[�AY�hAV�HAR�\AOhsALQ�AKO�AJ$�AH��AGK�ABE�A@=qA?�A>1'A<ffA:�uA8=qA6�+A5��A4E�A21'A/\)A,�!A*�/A)��A'�FA&Q�A%��A$�jA#A"��A"1'A!��A!&�A �/A z�A�A�-A�RA7LA�!A�/A��A��A��Av�A��A�!A�mAS�A�`A1Al�A��A~�Az�AbNA$�A�TA|�A��AI�AO�A
�DA	�TAA�AK�A�A��A^5AbA�#A�^AdZAA�/AI�At�A��A=qA�A"�A �HA ^5A �@���@��;@���@��@���@�t�@�ff@���@���@���@�@�r�@���@�9X@��@���@�\@��@�@�@��@�@���@�x�@��;@���@�u@�K�@�R@�ȴ@�@��u@�9X@�dZ@���@ޗ�@��@�r�@ۍP@ڗ�@�ff@��/@��
@׍P@�t�@��H@��@���@��T@�hs@ԋD@��@҇+@��@�  @Ͼw@�t�@��@�J@�&�@�Ĝ@�A�@�A�@� �@���@ɩ�@ɲ-@ɲ-@�hs@��/@���@�1'@�1'@�(�@Ǖ�@�n�@ř�@��@�1'@�ƨ@�33@�@��@��^@��h@��@�  @�l�@��!@�E�@���@��@�`B@��@���@�Q�@��@���@�|�@�l�@�S�@��@��\@�{@���@��@�1'@���@���@��@�l�@�33@���@��y@��R@�{@��@�&�@���@���@�z�@�I�@�(�@�1@��@�+@���@���@���@���@��\@�n�@�J@���@���@�G�@�%@���@�Z@�  @�ƨ@���@�dZ@�33@�
=@��!@�^5@�M�@��@��@���@���@�?}@���@��@�r�@�I�@���@���@��@��@���@�M�@�$�@��7@��@���@�Ĝ@��j@��@��D@�1'@��@�@��H@��H@��H@��H@��H@��H@��H@��y@�ȴ@�~�@�^5@�5?@�{@��@�hs@��@�j@�1@��P@�C�@��\@��@���@�p�@�&�@�Ĝ@��u@��@�z�@�r�@�Q�@�9X@� �@�  @���@�S�@�"�@�ȴ@��\@��#@�hs@�/@��@��`@��9@��D@�A�@�  @��F@���@���@��@�K�@���@�^5@�V@�{@���@�p�@�V@��`@��D@�  @�t�@�C�@�"�@��y@�=q@��@�@��T@���@���@�X@�&�@�%@���@��u@�bN@��m@���@�|�@�l�@�+@��@��+@�=q@�@���@��@�x�@�`B@�7L@��@�9X@��;@�ƨ@���@�+@���@�~�@�v�@�n�@�M�@�{@�@�X@�/@���@���@�Z@� �@���@��P@�C�@�o@���@���@�M�@��@�@��h@��@�Ĝ@��D@�bN@�A�@�ƨ@�ȴ@�n�@�$�@�{@��@��7@�hs@�G�@���@��D@�1'@��@��@���@�l�@��H@��\@�~�@�^5@�E�@�$�@�J@�{@�{@��@��@�J@���@�X@�7L@���@�Q�@�(�@�;@\)@~�@~E�@}�@}V@|��@|j@|1@{t�@{33@z��@z�\@z-@y��@y��@yhs@y%@xbN@xb@x  @x  @xb@w�;@w|�@wl�@w�@v�@v��@vff@v{@u��@u�-@u��@up�@t�/@tj@t(�@s��@sƨ@st�@s33@so@s@r�@r�@r��@r��@r��@r^5@rJ@q��@q��@q�@p  @o�@o��@o��@o��@o�P@o\)@oK�@n��@n��@nV@n@m�-@m�@m?}@l��@l�@k�m@ko@j�\@i�@i�7@i7L@h��@h��@h��@hA�@g�w@g�P@g\)@g�@f�y@f�R@f�+@fE�@e�h@d�@c@bn�@a�#@a�^@a��@aG�@`��@`A�@_l�@_;d@^�y@^ȴ@^��@^v�@^ff@]�-@\9X@[��@Z��@ZM�@Y�#@YG�@Y%@X��@X  @W��@W��@W;d@V�y@V�R@VV@U�T@U��@U�@Up�@U?}@T�/@Tz�@S��@S�@R�@R�\@R�@Qx�@P��@P�u@Pr�@Pb@O�w@O;d@Nȴ@N�+@NV@NE�@M�@M�@L��@L�/@L�j@Lj@L1@Kƨ@K�@Kt�@KS�@K"�@J�H@J�\@JJ@I��@I��@I�7@I�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�G�A�"�A�{A�1A���A���A��A��A��A��A��A��mA��TA��;A��A���AҴ9A҅A�A�A���A�p�A��AϺ^Aϡ�Aϟ�Aϡ�Aϗ�AρA�hsA�ZA�A�A�33A�$�A��A�  A���A�t�A��TA�v�A�A�A̸RA� �A�JA�K�A��AɬA�t�A�O�A�33A�
=A�^5Aǡ�A�Q�A��A��AƼjA�{A�E�A�7LA�33A�A��A�jA��
A�5?A���A�v�A��A��jA���A���A��jA���A�l�A��A�JA�A���A��mA��A�1A���A���A�jA�(�A��RA�M�A��TA���A�^5A�JA���A��hA�33A�  A��hA�bNA�A�A�Q�A��A��;A�dZA�^5A��#A�G�A�(�A��`A�K�A���A�9XA�E�A���A��A� �A�/A�t�A���A�JA���A��wA��FA���A�-A��A��uA�5?A~I�A{�Au�Ap{Aj�DAd5?A`A[�AY�hAV�HAR�\AOhsALQ�AKO�AJ$�AH��AGK�ABE�A@=qA?�A>1'A<ffA:�uA8=qA6�+A5��A4E�A21'A/\)A,�!A*�/A)��A'�FA&Q�A%��A$�jA#A"��A"1'A!��A!&�A �/A z�A�A�-A�RA7LA�!A�/A��A��A��Av�A��A�!A�mAS�A�`A1Al�A��A~�Az�AbNA$�A�TA|�A��AI�AO�A
�DA	�TAA�AK�A�A��A^5AbA�#A�^AdZAA�/AI�At�A��A=qA�A"�A �HA ^5A �@���@��;@���@��@���@�t�@�ff@���@���@���@�@�r�@���@�9X@��@���@�\@��@�@�@��@�@���@�x�@��;@���@�u@�K�@�R@�ȴ@�@��u@�9X@�dZ@���@ޗ�@��@�r�@ۍP@ڗ�@�ff@��/@��
@׍P@�t�@��H@��@���@��T@�hs@ԋD@��@҇+@��@�  @Ͼw@�t�@��@�J@�&�@�Ĝ@�A�@�A�@� �@���@ɩ�@ɲ-@ɲ-@�hs@��/@���@�1'@�1'@�(�@Ǖ�@�n�@ř�@��@�1'@�ƨ@�33@�@��@��^@��h@��@�  @�l�@��!@�E�@���@��@�`B@��@���@�Q�@��@���@�|�@�l�@�S�@��@��\@�{@���@��@�1'@���@���@��@�l�@�33@���@��y@��R@�{@��@�&�@���@���@�z�@�I�@�(�@�1@��@�+@���@���@���@���@��\@�n�@�J@���@���@�G�@�%@���@�Z@�  @�ƨ@���@�dZ@�33@�
=@��!@�^5@�M�@��@��@���@���@�?}@���@��@�r�@�I�@���@���@��@��@���@�M�@�$�@��7@��@���@�Ĝ@��j@��@��D@�1'@��@�@��H@��H@��H@��H@��H@��H@��H@��y@�ȴ@�~�@�^5@�5?@�{@��@�hs@��@�j@�1@��P@�C�@��\@��@���@�p�@�&�@�Ĝ@��u@��@�z�@�r�@�Q�@�9X@� �@�  @���@�S�@�"�@�ȴ@��\@��#@�hs@�/@��@��`@��9@��D@�A�@�  @��F@���@���@��@�K�@���@�^5@�V@�{@���@�p�@�V@��`@��D@�  @�t�@�C�@�"�@��y@�=q@��@�@��T@���@���@�X@�&�@�%@���@��u@�bN@��m@���@�|�@�l�@�+@��@��+@�=q@�@���@��@�x�@�`B@�7L@��@�9X@��;@�ƨ@���@�+@���@�~�@�v�@�n�@�M�@�{@�@�X@�/@���@���@�Z@� �@���@��P@�C�@�o@���@���@�M�@��@�@��h@��@�Ĝ@��D@�bN@�A�@�ƨ@�ȴ@�n�@�$�@�{@��@��7@�hs@�G�@���@��D@�1'@��@��@���@�l�@��H@��\@�~�@�^5@�E�@�$�@�J@�{@�{@��@��@�J@���@�X@�7L@���@�Q�@�(�@�;@\)@~�@~E�@}�@}V@|��@|j@|1@{t�@{33@z��@z�\@z-@y��@y��@yhs@y%@xbN@xb@x  @x  @xb@w�;@w|�@wl�@w�@v�@v��@vff@v{@u��@u�-@u��@up�@t�/@tj@t(�@s��@sƨ@st�@s33@so@s@r�@r�@r��@r��@r��@r^5@rJ@q��@q��@q�@p  @o�@o��@o��@o��@o�P@o\)@oK�@n��@n��@nV@n@m�-@m�@m?}@l��@l�@k�m@ko@j�\@i�@i�7@i7L@h��@h��@h��@hA�@g�w@g�P@g\)@g�@f�y@f�R@f�+@fE�@e�h@d�@c@bn�@a�#@a�^@a��@aG�@`��@`A�@_l�@_;d@^�y@^ȴ@^��@^v�@^ff@]�-@\9X@[��@Z��@ZM�@Y�#@YG�@Y%@X��@X  @W��@W��@W;d@V�y@V�R@VV@U�T@U��@U�@Up�@U?}@T�/@Tz�@S��@S�@R�@R�\@R�@Qx�@P��@P�u@Pr�@Pb@O�w@O;d@Nȴ@N�+@NV@NE�@M�@M�@L��@L�/@L�j@Lj@L1@Kƨ@K�@Kt�@KS�@K"�@J�H@J�\@JJ@I��@I��@I�7@I�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	B	B	B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	��B	�B	��B
B
+B
+B
+B
	7B
JB
PB
VB
bB
\B
\B
bB
oB
�B
&�B
N�B
p�B
}�B
��B
��B�B:^BN�B^5Bm�Bu�B{�B�B��B�jBǮB��B�B�5B��BJB%�B<jBH�BR�B[#B_;BaHBq�B�1B~�B�1B�bB�bB�bB�bB�uB��B�7B�=B�DB�PB�VB�7B�1B�%B�Bv�Br�Bn�Bm�Bm�Bm�BjBbNBbNB[#BG�BbB��B��B��B�)B��B�B�BB�;B�B��B�3B��B�DBiyB1'B �B
=B
��B
�B
�B
�sB
��B
�=B
� B
�B
�DB
�hB
ZB
2-B
�B
�B	��B	�jB	�JB	ffB	A�B	"�B	hB	PB	B�B�B�mB�ZB�NB�;B�;B�TB�/B�5B�)B�)B�B�B�
B��B��B��B��BɺBǮBƨBǮBƨBŢBÖB��B�wB�qB�jB�jB�^B�^B�RB�9B�'B�B�B�!B�LB�RB�wB��B�wB�wB�qB�jB�qB�dB�XB�XB�^B�^B�dB�qB��BȴBȴBƨB��BĜBǮB��B��B��B��B��B�
B�B�#B�B�)B�/B�5B�BB�;B�HB�sB�B�B�B�B��B��B��B��B��B��B��B	B	B	
=B	DB	
=B	DB	VB	oB	bB	\B	VB	VB	\B	bB	bB	bB	hB	bB	oB	hB	�B	�B	�B	�B	�B	 �B	"�B	+B	,B	,B	-B	2-B	7LB	:^B	:^B	@�B	@�B	?}B	?}B	>wB	<jB	@�B	B�B	C�B	C�B	C�B	D�B	E�B	G�B	G�B	H�B	J�B	P�B	Q�B	Q�B	R�B	S�B	VB	W
B	S�B	W
B	YB	ZB	[#B	cTB	jB	k�B	m�B	s�B	w�B	t�B	s�B	s�B	s�B	z�B	}�B	~�B	~�B	� B	�B	�B	�B	�B	�1B	�=B	�\B	�oB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�9B	�?B	�LB	�RB	�RB	�XB	�^B	�^B	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�BB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�NB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
PB
VB
\B
\B
\B
\B
hB
oB
uB
uB
uB
uB
{B
uB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
&�B
'�B
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
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
/B
0!B
/B
0!B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
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
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
;dB
;dB
;dB
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
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
G�B
F�B
G�B
G�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	B	B	B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	��B	�B	��B
B
+B
+B
+B
	7B
JB
PB
VB
bB
\B
\B
bB
oB
�B
&�B
N�B
p�B
}�B
��B
��B�B:^BN�B^5Bm�Bu�B{�B�B��B�jBǮB��B�B�5B��BJB%�B<jBH�BR�B[#B_;BaHBq�B�1B~�B�1B�bB�bB�bB�bB�uB��B�7B�=B�DB�PB�VB�7B�1B�%B�Bv�Br�Bn�Bm�Bm�Bm�BjBbNBbNB[#BG�BbB��B��B��B�)B��B�B�BB�;B�B��B�3B��B�DBiyB1'B �B
=B
��B
�B
�B
�sB
��B
�=B
� B
�B
�DB
�hB
ZB
2-B
�B
�B	��B	�jB	�JB	ffB	A�B	"�B	hB	PB	B�B�B�mB�ZB�NB�;B�;B�TB�/B�5B�)B�)B�B�B�
B��B��B��B��BɺBǮBƨBǮBƨBŢBÖB��B�wB�qB�jB�jB�^B�^B�RB�9B�'B�B�B�!B�LB�RB�wB��B�wB�wB�qB�jB�qB�dB�XB�XB�^B�^B�dB�qB��BȴBȴBƨB��BĜBǮB��B��B��B��B��B�
B�B�#B�B�)B�/B�5B�BB�;B�HB�sB�B�B�B�B��B��B��B��B��B��B��B	B	B	
=B	DB	
=B	DB	VB	oB	bB	\B	VB	VB	\B	bB	bB	bB	hB	bB	oB	hB	�B	�B	�B	�B	�B	 �B	"�B	+B	,B	,B	-B	2-B	7LB	:^B	:^B	@�B	@�B	?}B	?}B	>wB	<jB	@�B	B�B	C�B	C�B	C�B	D�B	E�B	G�B	G�B	H�B	J�B	P�B	Q�B	Q�B	R�B	S�B	VB	W
B	S�B	W
B	YB	ZB	[#B	cTB	jB	k�B	m�B	s�B	w�B	t�B	s�B	s�B	s�B	z�B	}�B	~�B	~�B	� B	�B	�B	�B	�B	�1B	�=B	�\B	�oB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�9B	�?B	�LB	�RB	�RB	�XB	�^B	�^B	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�BB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�NB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
PB
VB
\B
\B
\B
\B
hB
oB
uB
uB
uB
uB
{B
uB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
&�B
'�B
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
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
/B
0!B
/B
0!B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
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
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
;dB
;dB
;dB
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
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
G�B
F�B
G�B
G�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20180625171537  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180625081545  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180625081545  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180625081545  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180625081546  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180625081546  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180625081546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180625081546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180625081546  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180625081546                      G�O�G�O�G�O�                JA  ARUP                                                                        20180625091527                      G�O�G�O�G�O�                
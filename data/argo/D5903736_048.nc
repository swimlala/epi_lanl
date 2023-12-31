CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:31Z AOML 3.0 creation; 2016-05-31T19:14:32Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20140721230531  20160531121432  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               0A   AO  4051_7090_048                   2C  D   APEX                            5368                            041511                          846 @֬u�_@
1   @֬vE�@4�`A�7L�d�
=p��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    0A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffBffB   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D�fD�FfD�|�D��fD�fD�FfD�� D��3D���D�FfD�� D��3D�3D�,�D�I�D���D�3D�6fD�s3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @>�R@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�BQ�BQ�B�B'�B/�B7�B?�BG�BO�BW�B`Q�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C${C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Cf{Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt�RDy��D��D�E�D�|)D���D��D�E�D��\D�ҏD���D�E�D�\D�ҏD��D�,)D�H�D��)D��D�5�D�r�D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA�ĜA׺^A׸RA׶FA״9Aװ!Aװ!Aװ!Aװ!A׮A׮A׮A׬A׬Aש�A׬A�;dA�z�A���A��/A�;dA˃A��A�G�AŅA�jA�1'A�r�A��wA��-A�VA�-A��-A��HA���A�I�A�G�A�JA���A�l�A��A��^A�hsA��;A��A��PA�x�A�\)A��A���A�33A��A��\A���A�x�A���A��A��A��uA�VA�M�A��A�A���A���A�C�A��A�=qA��uA�$�A��A�A��A���A�ZA�/A�{A�JA�%A���A���A���A���A��A���A�9XA��9A��#A��TA�x�A�^5A��PA�^5A��wA�v�A���A�z�A��A���A���A�VA�x�A��-A��#A��A��A���A���A� �A���A�(�A��-A�{A��!A~{A{�Av�Au&�At�DAs|�Ap�uAo�Am��Al��Al(�Ak��Ak��Ak�^Ak�^Ak�Ak��Ak��Ak��Ak�hAk�Ak�7Ak�Akl�Aj�DAi�TAi�^AhE�Af�HAf^5Ae��Ac��Aa��A^�A]7LA\�A\�A\jA\bNA\{A[A["�AY��AY�AX=qAW�PAV��AT��AS`BAR1'AL�AK"�AJ^5AIC�AHI�AG�AB�A<��A<-A< �A<JA;��A;33A9�A8ȴA7�-A6A�A4�RA2{A.VA-�A,��A,bA+��A+S�A*ĜA*A�A*5?A*(�A*bA(A%�PA$JA!S�A��Al�Av�AA�-A33A�A��AK�A��Ar�A �A�A�/A�+AjAA�A�TA?}A
=A��AA;dA�FA1'A��A=qAƨA
ZA�jAA��A��AbNA�!Al�A �\@�\)@���@�t�@�7L@�  @�t�@�V@��@���@�?}@� �@�@�J@�  @��@�x�@�w@�R@���@�R@�h@��@�(�@�ƨ@�t�@��@���@�h@�j@�+@�`B@ܬ@�t�@��H@���@�ȴ@��@��`@ׅ@�n�@�5?@�@�`B@�&�@ԃ@�E�@��@�C�@���@�bN@˾w@ʰ!@�X@��
@Ł@���@þw@ÍP@�\)@�@��9@�|�@��-@�j@�33@��T@�Ĝ@�r�@�1@�M�@��@���@���@�{@��@�Ĝ@�bN@�Q�@��@�(�@�9X@�r�@��F@��@�j@�t�@��H@�v�@��@��@���@�S�@�@��\@��@�O�@���@�1'@�(�@��
@���@�G�@�Z@���@�ƨ@��F@�;d@��R@��\@�-@��h@�7L@���@�I�@��;@���@�t�@�l�@�K�@�o@��H@���@�M�@�p�@�`B@�/@���@��j@���@�bN@���@�o@���@��H@��R@��\@�^5@��-@�x�@�V@�b@��@��@���@��F@���@��@�dZ@���@�n�@�$�@�=q@�M�@�V@�V@�V@�V@�V@�E�@��@���@�x�@�x�@�x�@�G�@�Ĝ@�I�@�  @��m@���@��@���@��P@�|�@�\)@��@�bN@�z�@�I�@�Q�@�Z@�Z@�1'@���@���@���@��P@��@�dZ@�+@���@��!@��\@�v�@�v�@�n�@�M�@��@��-@��7@�x�@�`B@�/@��@��@���@��`@���@��j@��j@��j@�j@��@��@�t�@�"�@�@�~�@���@���@��7@�G�@��/@�Ĝ@���@�r�@�9X@�1@��@��P@�33@���@�ff@�M�@�=q@�=q@��@���@��@�hs@�G�@��@��j@�I�@��@���@�dZ@�o@��H@���@�n�@�V@�{@���@��@|�D@r��@m�h@dj@\j@X�`@K��@D�/@>��@8  @3dZ@.v�@&�+@"�@�@�#@1@1'@�m@	hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ƨA�ĜA׺^A׸RA׶FA״9Aװ!Aװ!Aװ!Aװ!A׮A׮A׮A׬A׬Aש�A׬A�;dA�z�A���A��/A�;dA˃A��A�G�AŅA�jA�1'A�r�A��wA��-A�VA�-A��-A��HA���A�I�A�G�A�JA���A�l�A��A��^A�hsA��;A��A��PA�x�A�\)A��A���A�33A��A��\A���A�x�A���A��A��A��uA�VA�M�A��A�A���A���A�C�A��A�=qA��uA�$�A��A�A��A���A�ZA�/A�{A�JA�%A���A���A���A���A��A���A�9XA��9A��#A��TA�x�A�^5A��PA�^5A��wA�v�A���A�z�A��A���A���A�VA�x�A��-A��#A��A��A���A���A� �A���A�(�A��-A�{A��!A~{A{�Av�Au&�At�DAs|�Ap�uAo�Am��Al��Al(�Ak��Ak��Ak�^Ak�^Ak�Ak��Ak��Ak��Ak�hAk�Ak�7Ak�Akl�Aj�DAi�TAi�^AhE�Af�HAf^5Ae��Ac��Aa��A^�A]7LA\�A\�A\jA\bNA\{A[A["�AY��AY�AX=qAW�PAV��AT��AS`BAR1'AL�AK"�AJ^5AIC�AHI�AG�AB�A<��A<-A< �A<JA;��A;33A9�A8ȴA7�-A6A�A4�RA2{A.VA-�A,��A,bA+��A+S�A*ĜA*A�A*5?A*(�A*bA(A%�PA$JA!S�A��Al�Av�AA�-A33A�A��AK�A��Ar�A �A�A�/A�+AjAA�A�TA?}A
=A��AA;dA�FA1'A��A=qAƨA
ZA�jAA��A��AbNA�!Al�A �\@�\)@���@�t�@�7L@�  @�t�@�V@��@���@�?}@� �@�@�J@�  @��@�x�@�w@�R@���@�R@�h@��@�(�@�ƨ@�t�@��@���@�h@�j@�+@�`B@ܬ@�t�@��H@���@�ȴ@��@��`@ׅ@�n�@�5?@�@�`B@�&�@ԃ@�E�@��@�C�@���@�bN@˾w@ʰ!@�X@��
@Ł@���@þw@ÍP@�\)@�@��9@�|�@��-@�j@�33@��T@�Ĝ@�r�@�1@�M�@��@���@���@�{@��@�Ĝ@�bN@�Q�@��@�(�@�9X@�r�@��F@��@�j@�t�@��H@�v�@��@��@���@�S�@�@��\@��@�O�@���@�1'@�(�@��
@���@�G�@�Z@���@�ƨ@��F@�;d@��R@��\@�-@��h@�7L@���@�I�@��;@���@�t�@�l�@�K�@�o@��H@���@�M�@�p�@�`B@�/@���@��j@���@�bN@���@�o@���@��H@��R@��\@�^5@��-@�x�@�V@�b@��@��@���@��F@���@��@�dZ@���@�n�@�$�@�=q@�M�@�V@�V@�V@�V@�V@�E�@��@���@�x�@�x�@�x�@�G�@�Ĝ@�I�@�  @��m@���@��@���@��P@�|�@�\)@��@�bN@�z�@�I�@�Q�@�Z@�Z@�1'@���@���@���@��P@��@�dZ@�+@���@��!@��\@�v�@�v�@�n�@�M�@��@��-@��7@�x�@�`B@�/@��@��@���@��`@���@��j@��j@��j@�j@��@��@�t�@�"�@�@�~�@���@���@��7@�G�@��/@�Ĝ@���@�r�@�9X@�1@��@��P@�33@���@�ff@�M�@�=q@�=q@��@���@��@�hs@�G�@��@��j@�I�@��@���@�dZ@�o@��H@���@�n�@�V@�{@���@��@|�D@r��@m�h@dj@\j@X�`@K��@D�/@>��@8  @3dZ@.v�@&�+@"�@�@�#@1@1'@�m@	hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�RB�LB�LB�LB�LB�LB�RB�RB�RB�RB�RB�XB�XB�XB�^B�dB��B�sB��B�B�HB��BŢB�wB�9B��B��B��B��B��B��B��B��B�{B�hB�bB�bB�JB�7B�1B�+B�B�B�B� B}�B|�B{�Bz�Bw�Bs�Bp�Bm�BhsB_;B[#BT�BK�BH�BD�B=qB5?B.B!�BhBB��B��B�B�sB�TB�BB�5B�)B�#B�B�B��B��B��B��B��B��B��B��B��B�?B�B��B�PBt�BQ�BB�B/B#�B�B��B�^B�%Br�BdZBXBM�B@�B0!BuB
��B
ǮB
��B
�uB
�DB
�B
|�B
s�B
_;B
H�B
6FB
�B
uB
\B
1B	��B	�B	�B	�mB	�TB	�HB	�HB	�HB	�HB	�HB	�BB	�BB	�BB	�BB	�;B	�;B	�;B	�5B	�B	�
B	��B	��B	ǮB	ĜB	�}B	�FB	�B	��B	��B	��B	��B	��B	��B	��B	�{B	�hB	�DB	�+B	�B	~�B	y�B	o�B	gmB	ZB	>wB	33B	.B	#�B	�B	\B��B�B�B�
B�
B��B��B��BȴBÖB�wB�XB�FB�!B�B��B��B�B�B�B�B�B�B��B��B��B��B��B�uB�bB�JB�=B�1B�%B�B~�B|�B{�Bz�By�Bx�Bw�Bw�Bv�Bv�Bu�Bt�Bs�Bq�Bp�Bm�BjBhsBe`BdZBaHB_;B^5B\)BZBVBVBS�BP�BN�BM�BL�BL�BK�BK�BJ�BK�BL�BL�BK�BH�BF�BE�BD�BD�BC�B?}B<jB@�BB�BB�BF�BI�BI�BI�BM�BP�BQ�BP�BL�BG�BD�BC�BD�BF�BF�BL�BM�BO�BT�BT�BW
BW
BW
BVBVB[#BaHBhsBhsBhsBhsBhsBhsBffBgmBgmBgmBffBiyBl�Bq�Bx�B}�B�B�B�B�B�JB��B��B��B��B��B��B��B�B�^B�wB��BĜBƨB��B��B�B�B�#B�)B�5B�HB�`B�B�B�B�B��B��B��B��B��B	B	+B	DB	VB	\B	\B	{B	�B	�B	�B	 �B	"�B	%�B	(�B	.B	2-B	49B	5?B	8RB	?}B	A�B	B�B	E�B	I�B	N�B	[#B	]/B	_;B	bNB	ffB	iyB	jB	jB	jB	jB	k�B	l�B	s�B	y�B	~�B	�B	�%B	�%B	�+B	�1B	�7B	�=B	�=B	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�9B	�9B	�?B	�?B	�?B	�FB	�LB	�XB	��B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�TB	�TB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
\B
�B
�B
$�B
,B
1'B
9XB
E�B
J�B
O�B
T�B
XB
_;B
bNB
dZB
gmB
l�B
p�B
u�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�XB�UB�UB�UB�TB�TB�\B�\B�^B�\B�\B�aB�cB�aB�eB�jB��B�B��B�B�TB�BŨB�}B�BB��B��B��B��B��B��B��B��B��B�pB�hB�lB�TB�>B�9B�1B�$B�B�B�B}�B|�B{�Bz�Bw�Bs�Bp�Bm�BhxB_BB['BUBK�BH�BD�B=wB5FB.B!�BjB$B��B��B�B�tB�XB�GB�;B�,B�'B�B�	B��B��B��B��B��B��B��B��B��B�@B�
B��B�RBt�BQ�BB�B/B#�B�B��B�aB�'Br�BdYBXBM�B@�B0'B{B
��B
ǲB
��B
�zB
�KB
�'B
|�B
s�B
_CB
H�B
6OB
�B
�B
eB
;B	��B	�B	�B	�yB	�`B	�QB	�QB	�QB	�SB	�SB	�KB	�LB	�MB	�MB	�EB	�EB	�EB	�@B	�)B	�B	�B	��B	ǻB	ĨB	��B	�QB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�RB	�:B	� B	B	y�B	o�B	g}B	Z,B	>�B	3DB	.%B	#�B	�B	pB��B�)B�$B�!B� B�B�B��B��BìB��B�mB�[B�9B� B�B�B�+B�2B�)B�#B�"B�B�B��B��B��B��B��B�yB�bB�TB�IB�;B�*BB}B| Bz�By�Bx�Bw�Bw�Bv�Bv�Bu�Bt�Bs�Bq�Bp�Bm�Bj�Bh�BexBdsBacB_UB^OB\BBZ3BV BVBTBQBN�BM�BL�BL�BK�BK�BJ�BK�BL�BL�BK�BH�BF�BE�BD�BD�BC�B?�B<�B@�BB�BB�BF�BI�BI�BI�BM�BP�BRBQ BL�BG�BD�BC�BD�BF�BF�BL�BM�BO�BUBUBW$BW$BW"BVBVB[=BaaBh�Bh�Bh�Bh�Bh�Bh�Bf�Bg�Bg�Bg�Bf~Bi�Bl�Bq�Bx�B~
B�B�B�B�'B�`B��B��B��B��B��B��B��B�1B�tB��B��BıBƺB��B�B�B�'B�6B�<B�IB�]B�qB�B�B�B��B��B��B��B��B��B	B	=B	UB	gB	nB	nB	�B	�B	�B	�B	 �B	"�B	%�B	)B	.$B	2>B	4IB	5OB	8aB	?�B	A�B	B�B	E�B	I�B	N�B	[1B	]<B	_KB	b\B	fqB	i�B	j�B	j�B	j�B	j�B	k�B	l�B	s�B	y�B	B	�+B	�/B	�0B	�7B	�<B	�DB	�HB	�JB	�aB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�2B	�DB	�CB	�IB	�KB	�IB	�QB	�VB	�aB	��B	ŬB	ȽB	��B	��B	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�1B	�8B	�6B	�@B	�AB	�EB	�EB	�EB	�CB	�KB	�\B	�]B	�jB	�rB	�tB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
B
B
dB
�B
�B
$�B
,B
1.B
9^B
E�B
J�B
O�B
UB
XB
_AB
bUB
d`B
gtB
l�B
p�B
u�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214322016053112143220160531121432  AO  ARCAADJP                                                                    20140721230531    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230531  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230531  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121432  IP                  G�O�G�O�G�O�                
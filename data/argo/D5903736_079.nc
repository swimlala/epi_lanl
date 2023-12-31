CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:47Z AOML 3.0 creation; 2016-05-31T19:14:37Z UW 3.1 conversion     
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
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20140721230547  20160531121437  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               OA   AO  4051_7090_079                   2C  D   APEX                            5368                            041511                          846 @���]��1   @��ˏ+�@3� ě���e$I�^5?1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    OA   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs�fDt  Dt� Dt��Dy� D�3D�<�D�y�D��3D�fD�L�D�� D��3D�3D�S3D��fDǹ�D�	�D�9�Dڠ D��fD�fD�@ D�y�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @>�R@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B`Q�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C�GC��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�DsDs�Ds��Dt~�Dt�Dy��D��D�<)D�x�D�ҏD��D�L)D�\D�ҏD��D�R�D���DǸ�D��D�8�Dڟ\D���D��D�?\D�x�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�n�A�p�A�XA�-A��A���Aٝ�Aُ\Aى7Aه+AكA�~�A�x�A�p�A�XAח�A�AԓuA���A�G�A���A�E�A���AуA��A�r�A�AϑhA�oAΗ�A�"�A�A̓A�S�A�1A�v�A�ȴA���A�;dA���A�;dAŇ+A�"�A���A���A�$�A�  A��^A�I�A�JA�A�  A�l�A�x�A��#A��A�dZA�\)A�&�A��
A���A�A�r�A���A�=qA�A���A���A��A�ĜA�l�A��A���A��HA�C�A�/A�$�A���A���A��\A�ZA��A��A���A��uA�dZA���A��A�O�A���A��-A�7LA�|�A���A��A�z�A��A�$�A�ZA��hA��A�A��A�x�A��A��uA���A��A�r�A�ĜA�oA���A��;A���A���A�=qA���A���A��A�|�A��A���A��RA�n�A���A��A��A�-A���A���A�l�A}�TA}/A|$�A{7LAzȴAx�RAu�hArr�Ao��Am��Ak&�Af��Ad��Ac�AbbNAa�7A`9XA^�A\��AZ��AX�\AWhsAV��AVI�AU�AU�AU&�AS�AP{AM33AH~�AC�wA?�7A<  A:jA9/A8�uA8JA7oA5x�A4��A3�A2ȴA1�mA1+A0�A/��A.��A.Q�A.JA-t�A,ĜA,A�A+�^A+�A)p�A'�
A&ffA$bNA!dZA �A �A�A��A�hA�A�A�+A��A�uA�A�A�HA1A�A1A�AhsAbAn�Ax�A
�A	C�A�AM�A$�A{A7LA�Al�A/A�A+A�AVA5?Ap�AA�\AA�A��A��AXAVA ��A ��A ��A ��A {@�G�@���@��\@��@��@�bN@���@��F@�"�@���@��-@�l�@��@��@��@�@�ƨ@�@�\)@�;d@��@�~�@�^5@�5?@�$�@��@�?}@��@�G�@�X@�p�@�hs@�D@�w@�^5@��@�@�G�@�Ĝ@��@�+@��@�t�@�=q@ٺ^@�p�@ٙ�@���@ج@�1@�ff@Ձ@��@ёh@�?}@�x�@��#@�V@�v�@�\)@Η�@�M�@�n�@���@ċD@�Q�@� �@�  @��@��;@��
@î@�
=@�ȴ@�~�@�@�z�@�K�@�$�@���@�&�@��@�Q�@�(�@� �@�b@�  @��m@�ƨ@��@�@��^@��@�X@�O�@�/@��@��`@���@�Q�@�|�@�33@��y@�^5@�$�@��@��@�ȴ@�n�@��@���@��u@���@��u@��;@�o@�-@���@��h@�7L@�&�@��@� �@��
@�K�@��y@���@��D@��@��u@���@��@��9@��@��@��u@�j@�\)@��!@�{@��^@��7@��7@�X@�V@��`@��@�z�@�9X@��@���@�33@���@���@�7L@��u@�Q�@�S�@�o@��y@�~�@��@��#@���@��@�z�@�ƨ@�ƨ@���@�S�@��H@�M�@���@��T@��T@��T@��#@���@�X@�Ĝ@���@�Q�@�Z@� �@�S�@���@�n�@�E�@�5?@�@�O�@��/@��j@�j@�I�@�9X@�1'@��m@�ƨ@��F@�\)@��@���@���@���@��\@�~�@�n�@�@�&�@��@�Ĝ@�1'@��w@���@��@�dZ@�C�@�+@��@�
=@�
=@�@��@��R@��\@�$�@�p�@�?}@�V@���@�Z@�1@��w@��@�\)@���@���@�ff@��#@���@��7@�`B@��`@��@�I�@���@�l�@�+@��y@�O�@��@{"�@s��@l(�@aG�@\�j@S�
@K��@CdZ@<��@65?@1��@,z�@&�R@l�@��@|�@��@x�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�n�A�p�A�XA�-A��A���Aٝ�Aُ\Aى7Aه+AكA�~�A�x�A�p�A�XAח�A�AԓuA���A�G�A���A�E�A���AуA��A�r�A�AϑhA�oAΗ�A�"�A�A̓A�S�A�1A�v�A�ȴA���A�;dA���A�;dAŇ+A�"�A���A���A�$�A�  A��^A�I�A�JA�A�  A�l�A�x�A��#A��A�dZA�\)A�&�A��
A���A�A�r�A���A�=qA�A���A���A��A�ĜA�l�A��A���A��HA�C�A�/A�$�A���A���A��\A�ZA��A��A���A��uA�dZA���A��A�O�A���A��-A�7LA�|�A���A��A�z�A��A�$�A�ZA��hA��A�A��A�x�A��A��uA���A��A�r�A�ĜA�oA���A��;A���A���A�=qA���A���A��A�|�A��A���A��RA�n�A���A��A��A�-A���A���A�l�A}�TA}/A|$�A{7LAzȴAx�RAu�hArr�Ao��Am��Ak&�Af��Ad��Ac�AbbNAa�7A`9XA^�A\��AZ��AX�\AWhsAV��AVI�AU�AU�AU&�AS�AP{AM33AH~�AC�wA?�7A<  A:jA9/A8�uA8JA7oA5x�A4��A3�A2ȴA1�mA1+A0�A/��A.��A.Q�A.JA-t�A,ĜA,A�A+�^A+�A)p�A'�
A&ffA$bNA!dZA �A �A�A��A�hA�A�A�+A��A�uA�A�A�HA1A�A1A�AhsAbAn�Ax�A
�A	C�A�AM�A$�A{A7LA�Al�A/A�A+A�AVA5?Ap�AA�\AA�A��A��AXAVA ��A ��A ��A ��A {@�G�@���@��\@��@��@�bN@���@��F@�"�@���@��-@�l�@��@��@��@�@�ƨ@�@�\)@�;d@��@�~�@�^5@�5?@�$�@��@�?}@��@�G�@�X@�p�@�hs@�D@�w@�^5@��@�@�G�@�Ĝ@��@�+@��@�t�@�=q@ٺ^@�p�@ٙ�@���@ج@�1@�ff@Ձ@��@ёh@�?}@�x�@��#@�V@�v�@�\)@Η�@�M�@�n�@���@ċD@�Q�@� �@�  @��@��;@��
@î@�
=@�ȴ@�~�@�@�z�@�K�@�$�@���@�&�@��@�Q�@�(�@� �@�b@�  @��m@�ƨ@��@�@��^@��@�X@�O�@�/@��@��`@���@�Q�@�|�@�33@��y@�^5@�$�@��@��@�ȴ@�n�@��@���@��u@���@��u@��;@�o@�-@���@��h@�7L@�&�@��@� �@��
@�K�@��y@���@��D@��@��u@���@��@��9@��@��@��u@�j@�\)@��!@�{@��^@��7@��7@�X@�V@��`@��@�z�@�9X@��@���@�33@���@���@�7L@��u@�Q�@�S�@�o@��y@�~�@��@��#@���@��@�z�@�ƨ@�ƨ@���@�S�@��H@�M�@���@��T@��T@��T@��#@���@�X@�Ĝ@���@�Q�@�Z@� �@�S�@���@�n�@�E�@�5?@�@�O�@��/@��j@�j@�I�@�9X@�1'@��m@�ƨ@��F@�\)@��@���@���@���@��\@�~�@�n�@�@�&�@��@�Ĝ@�1'@��w@���@��@�dZ@�C�@�+@��@�
=@�
=@�@��@��R@��\@�$�@�p�@�?}@�V@���@�Z@�1@��w@��@�\)@���@���@�ff@��#@���@��7@�`B@��`@��@�I�@���@�l�@�+@��y@�O�@��@{"�@s��@l(�@aG�@\�j@S�
@K��@CdZ@<��@65?@1��@,z�@&�R@l�@��@|�@��@x�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�BuB33BaHBgmBm�Br�Bw�B�B�bB��B��B��B��B��B�B�B�3B�dB��BǮB��B�
B�`B1B �B%�B+B/BW
Bv�B�B�VB�bB�uB��B�B�LB�qBŢB��B��B��BƨB�9B�dB��B�B�B�TB�B�sB�fB�fB�fB�`B�ZB�HB�5B�#B�B�
B�#B�#B�B��B��B��BÖB�?B�B�B��B��B�Bu�Bk�BffB\)BP�BE�B7LB(�B �B#�B �BVB1B%BB��B��B�B��BǮB�'B��B�DBH�BDB�B�;B��B��BiyBP�B,BuBPB
=BB
��B
�ZB
ÖB
�dB
�-B
��B
s�B
_;B
YB
Q�B
J�B
D�B
49B
�B
	7B	��B	�B	�#B	ƨB	�dB	�9B	�!B	�B	��B	��B	�{B	�DB	�B	|�B	y�B	x�B	v�B	t�B	p�B	e`B	S�B	@�B	 �B	B�B�`B�BB�/B�#B�B��B��BɺBǮBŢBÖB��B�}B�wB�jB�jB�dB�^B�XB�XB�RB�LB�9B�B��B��B��B��B��B��B��B��B��B�uB�VB��B�{B�{B�{B�{B�uB�oB�uB��B��B�{B��B��B��B��B��B�B�'B�3BƨBĜB��B��B��BÖBȴB��B��B��B��B��B��B��B��B��B�
B�B�#B�)B�)B�BB�B�B��B��B��B��B��B��B��B��B��B��B	B	B	B	  B	B	B	B	%B	+B	1B	
=B	
=B	DB	
=B	JB	{B	�B	�B	�B	$�B	-B	.B	1'B	2-B	2-B	33B	2-B	1'B	1'B	(�B	&�B	(�B	+B	,B	/B	1'B	/B	-B	+B	)�B	(�B	)�B	/B	49B	8RB	<jB	?}B	L�B	@�B	1'B	0!B	8RB	?}B	B�B	D�B	D�B	E�B	E�B	E�B	F�B	I�B	K�B	K�B	L�B	K�B	L�B	N�B	N�B	P�B	Q�B	S�B	S�B	T�B	T�B	T�B	VB	W
B	W
B	YB	]/B	]/B	\)B	\)B	\)B	\)B	_;B	_;B	dZB	jB	jB	hsB	hsB	hsB	iyB	hsB	ffB	e`B	iyB	k�B	l�B	l�B	l�B	q�B	t�B	w�B	y�B	|�B	�+B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�?B	�LB	�LB	�RB	�XB	�^B	�^B	�dB	�jB	�qB	��B	��B	��B	B	��B	��B	��B	�}B	�qB	�dB	�XB	�RB	�RB	�XB	�RB	�RB	�XB	�jB	�wB	�}B	��B	B	ĜB	ǮB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�/B	�5B	�;B	�HB	�NB	�NB	�NB	�ZB	�ZB	�ZB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
DB
uB
�B
!�B
+B
/B
6FB
=qB
C�B
H�B
N�B
R�B
W
B
]/B
cTB
ffB
jB
l�B
o�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�BwB32BaHBgoBm�Br�Bw�B�B�eB��B��B��B��B��B�B�B�4B�fB��BǴB��B�B�fB7B �B%�B+B/"BWBv�B�B�[B�iB�}B��B�"B�RB�xBŬB��B��B��BưB�>B�mB��B� B�B�]B�B�|B�pB�mB�pB�hB�fB�PB�@B�/B�B�B�0B�*B�*B��B��B��BáB�GB�B�B��B��B�Bu�Bk�BflB\-BP�BE�B7SB(�B �B#�B �BWB2B)BB��B��B�B�BǱB�*B��B�IBH�BCB�B�=B��B��Bi{BP�B,
BxBRB
CBB
��B
�]B
ÚB
�jB
�4B
��B
s�B
_DB
Y!B
Q�B
J�B
D�B
4BB
�B
	AB	��B	�B	�/B	ƳB	�pB	�FB	�-B	�B	��B	��B	��B	�TB	�B	|�B	y�B	x�B	v�B	t�B	p�B	epB	TB	@�B	 �B	B��B�sB�VB�BB�7B�&B�	B��B��B��BŴBëB��B��B��B��B�B�yB�tB�oB�kB�hB�dB�PB�1B�B��B��B��B��B��B��B��B��B��B�mB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�$B�<B�JBƻBĳB��B��B��BîB��B��B��B��B��B��B��B��B��B�B�B�*B�8B�>B�<B�TB�B�B��B��B��B��B�B�
B�
B�B� B�B	%B	#B	B	 B	B	B	,B	7B	>B	CB	
NB	
MB	TB	
NB	ZB	�B	�B	�B	�B	$�B	-B	.$B	19B	29B	2>B	3DB	2;B	16B	16B	)B	&�B	)B	+B	,B	/+B	18B	/-B	-B	+B	*B	)B	*B	/,B	4KB	8bB	<xB	?�B	L�B	@�B	16B	0/B	8bB	?�B	B�B	D�B	D�B	E�B	E�B	E�B	F�B	I�B	K�B	K�B	L�B	K�B	L�B	N�B	N�B	P�B	Q�B	TB	TB	UB	UB	U
B	VB	WB	WB	Y'B	]<B	]=B	\:B	\9B	\8B	\8B	_HB	_JB	dgB	j�B	j�B	h�B	h�B	h�B	i�B	h�B	ftB	enB	i�B	k�B	l�B	l�B	l�B	q�B	t�B	w�B	y�B	|�B	�7B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�2B	�JB	�YB	�YB	�[B	�cB	�kB	�iB	�qB	�uB	�}B	��B	��B	��B	B	��B	��B	��B	��B	�|B	�qB	�bB	�]B	�\B	�cB	�ZB	�[B	�aB	�uB	��B	��B	��B	B	ĨB	ǹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�(B	�)B	�'B	�*B	�'B	�;B	�@B	�EB	�QB	�UB	�VB	�YB	�eB	�cB	�eB	�qB	�xB	�wB	�|B	�|B	�}B	�}B	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
B
MB
~B
�B
!�B
+
B
/!B
6OB
={B
C�B
H�B
N�B
R�B
WB
]5B
c[B
flB
j�B
l�B
o�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214372016053112143720160531121437  AO  ARCAADJP                                                                    20140721230547    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230547  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230547  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121437  IP                  G�O�G�O�G�O�                
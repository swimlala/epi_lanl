CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-09-30T05:01:38Z AOML 3.0 creation; 2016-05-31T19:14:39Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20140930050138  20160531121439  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ZA   AO  4051_7090_090                   2C  D   APEX                            5368                            041511                          846 @���ۿ�1   @��$��@3�-�d�?|�h1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ZA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffBA��BH  BP  BW��B_��BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD��D�I�D�|�D�� D��D�9�D�y�D���D�3D�9�D��3D��3D��D�S3Dډ�D���D���D�,�D�S31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��\@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B8Q�BA�BG�BO�BW�B_�BhQ�Bo�Bw�B�B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C{C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�DD~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��DtxRDy�D�)D�H�D�|)D��\D�)D�8�D�x�D��)D��D�8�D���D�ҏD��D�R�Dڈ�D��)D��)D�,)D�R�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�G�A�=qA�9XA�7LA�9XA�9XA�;dA�9XA�9XA�;dA�9XA�;dA�;dA�;dA�;dA�?}A�=qA�?}A�E�A�K�A�O�A�\)A�JA�/A�r�A݅Aڙ�A�1'Aӧ�A��/A�E�A��;A�~�A�bA���A��A��A�ƨA�O�A�K�AĴ9A���A�VA��jA���A�t�A��A��A��9A���A�%A�\)A���A���A��A�x�A���A�A�C�A�1A���A��A�9XA���A�33A���A��A�?}A�dZA�%A���A���A��A��PA�Q�A�E�A�hsA���A��yA�bA���A��`A�dZA� �A�S�A��`A�
=A��A�33A��A�+A��^A�^5A�`BA�v�A��A���A�A���A���A�  A�x�A�ffA�1'A��`A���A�JA�ZA��A|�/Az5?AyAy`BAvE�Au�;Aq;dAnbAl�Ak�hAj�AhM�AgS�Af(�Ad�yAdJA`��A^�A[��AZ�AW�mAT��AR�AR�AQ|�AN��AM��AL��ALbNAK��AJ��AI��AH�AG��AG|�AF��AF9XAE�AEXAD�`ADM�A@-A>9XA<�+A:v�A8��A6�A6v�A5�hA4�A4A2z�A1��A/�A.M�A-��A-K�A+�A*�uA)A)7LA(�\A'��A&�9A%%A#�A#33A"{A �yA M�A��A��A;dA�HAbAS�A�`AA�A�FA?}AA�A`BA  A7LAz�A$�AĜA/AI�Al�A��A��A;dA��A{AVA	��A	�#A	A�DAA�A��AG�A�#A�+Ar�Ap�A%A ��A n�@��m@�^5@�1'@��@�1'@�z�@��@���@��H@���@�|�@�$�@�X@��@��@��@�
=@�+@�v�@�j@�A�@홚@�=q@���@��@�"�@�t�@�K�@�P@�(�@�Z@�1'@��m@�t�@��@�$�@���@�7@�/@��@���@��T@���@߾w@�o@�v�@�hs@܋D@�dZ@�@٩�@ش9@׮@�/@��@ӕ�@�K�@�|�@ӕ�@�|�@�{@�p�@�G�@Ѓ@���@ϕ�@��@̼j@�I�@�C�@�M�@�7L@�v�@�J@�{@�bN@î@�l�@���@�"�@��7@Å@��T@�5?@��@���@�=q@��#@��^@��u@��@��@�+@���@�@�p�@���@�j@�  @���@�l�@�;d@���@�v�@�{@��@��-@��7@�G�@��/@��u@�bN@�1@���@��@���@��@�x�@��`@��@�r�@�r�@�z�@��@�j@�Q�@��P@��H@��@��^@�p�@��@���@��j@�r�@� �@��;@�dZ@��@�ȴ@��@�ȴ@���@�E�@�J@��#@�p�@�/@��@���@�`B@��@�O�@�V@�z�@��m@���@�|�@�K�@�"�@���@���@��\@��\@��+@�ff@��@��#@��h@�/@��`@��D@�A�@� �@��@���@��P@���@���@��F@��
@�bN@�(�@���@���@��@��@���@���@���@�C�@��@�ȴ@��!@�ff@��h@��@��D@�  @��F@�t�@�l�@�l�@�l�@�l�@�t�@�t�@�t�@�dZ@�;d@�@��@���@�ȴ@��R@���@��T@�O�@�/@���@�b@�o@�n�@�J@�J@�@���@���@��7@�`B@�X@�G�@��9@� �@��m@�ƨ@��@�t�@�l�@�S�@�K�@�K�@�K�@�C�@�33@��!@�V@��@�p�@���@���@��D@�9X@���@�ƨ@���@��@�l�@�\)@�;d@�
=@��y@��+@�=q@��@���@��T@��#@��-@�A�@�dZ@{o@p  @i��@a�@Y��@P�u@IX@BM�@:�@5�h@/
=@)�@"�H@��@�+@��@5?@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�=qA�9XA�7LA�9XA�9XA�;dA�9XA�9XA�;dA�9XA�;dA�;dA�;dA�;dA�?}A�=qA�?}A�E�A�K�A�O�A�\)A�JA�/A�r�A݅Aڙ�A�1'Aӧ�A��/A�E�A��;A�~�A�bA���A��A��A�ƨA�O�A�K�AĴ9A���A�VA��jA���A�t�A��A��A��9A���A�%A�\)A���A���A��A�x�A���A�A�C�A�1A���A��A�9XA���A�33A���A��A�?}A�dZA�%A���A���A��A��PA�Q�A�E�A�hsA���A��yA�bA���A��`A�dZA� �A�S�A��`A�
=A��A�33A��A�+A��^A�^5A�`BA�v�A��A���A�A���A���A�  A�x�A�ffA�1'A��`A���A�JA�ZA��A|�/Az5?AyAy`BAvE�Au�;Aq;dAnbAl�Ak�hAj�AhM�AgS�Af(�Ad�yAdJA`��A^�A[��AZ�AW�mAT��AR�AR�AQ|�AN��AM��AL��ALbNAK��AJ��AI��AH�AG��AG|�AF��AF9XAE�AEXAD�`ADM�A@-A>9XA<�+A:v�A8��A6�A6v�A5�hA4�A4A2z�A1��A/�A.M�A-��A-K�A+�A*�uA)A)7LA(�\A'��A&�9A%%A#�A#33A"{A �yA M�A��A��A;dA�HAbAS�A�`AA�A�FA?}AA�A`BA  A7LAz�A$�AĜA/AI�Al�A��A��A;dA��A{AVA	��A	�#A	A�DAA�A��AG�A�#A�+Ar�Ap�A%A ��A n�@��m@�^5@�1'@��@�1'@�z�@��@���@��H@���@�|�@�$�@�X@��@��@��@�
=@�+@�v�@�j@�A�@홚@�=q@���@��@�"�@�t�@�K�@�P@�(�@�Z@�1'@��m@�t�@��@�$�@���@�7@�/@��@���@��T@���@߾w@�o@�v�@�hs@܋D@�dZ@�@٩�@ش9@׮@�/@��@ӕ�@�K�@�|�@ӕ�@�|�@�{@�p�@�G�@Ѓ@���@ϕ�@��@̼j@�I�@�C�@�M�@�7L@�v�@�J@�{@�bN@î@�l�@���@�"�@��7@Å@��T@�5?@��@���@�=q@��#@��^@��u@��@��@�+@���@�@�p�@���@�j@�  @���@�l�@�;d@���@�v�@�{@��@��-@��7@�G�@��/@��u@�bN@�1@���@��@���@��@�x�@��`@��@�r�@�r�@�z�@��@�j@�Q�@��P@��H@��@��^@�p�@��@���@��j@�r�@� �@��;@�dZ@��@�ȴ@��@�ȴ@���@�E�@�J@��#@�p�@�/@��@���@�`B@��@�O�@�V@�z�@��m@���@�|�@�K�@�"�@���@���@��\@��\@��+@�ff@��@��#@��h@�/@��`@��D@�A�@� �@��@���@��P@���@���@��F@��
@�bN@�(�@���@���@��@��@���@���@���@�C�@��@�ȴ@��!@�ff@��h@��@��D@�  @��F@�t�@�l�@�l�@�l�@�l�@�t�@�t�@�t�@�dZ@�;d@�@��@���@�ȴ@��R@���@��T@�O�@�/@���@�b@�o@�n�@�J@�J@�@���@���@��7@�`B@�X@�G�@��9@� �@��m@�ƨ@��@�t�@�l�@�S�@�K�@�K�@�K�@�C�@�33@��!@�V@��@�p�@���@���@��D@�9X@���@�ƨ@���@��@�l�@�\)@�;d@�
=@��y@��+@�=q@��@���@��T@��#@��-@�A�@�dZ@{o@p  @i��@a�@Y��@P�u@IX@BM�@:�@5�h@/
=@)�@"�H@��@�+@��@5?@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBD�BE�BD�BD�BD�BD�BD�BD�BD�BD�BD�BE�BE�BE�BE�BG�BG�BG�BI�BK�BL�BT�B�DB�ZB��B1B#�BJ�BO�BQ�BT�BR�BQ�BR�BR�BR�BQ�BQ�BP�BYBVBR�BW
BZB`BB^5BiyBv�B�LB�jB�FB�-B�B�B��B��B{�BQ�BA�B)�B�BDB��B��B�#BǮB��B�}B�}B��B�qB�wB�RB��B��B�+Bk�B[#BM�B<jB%�B�BVB��B�B�NB�B��B�}B�B��B�BhsBT�BC�B7LB&�BbB
=B
��B
�B
�NB
�;B
�B
��B
�B
�B
t�B
_;B
I�B
7LB
2-B
.B
�B
�B	��B	�`B	�B	�B	ȴB	�jB	�-B	�B	�B	��B	��B	�B	u�B	gmB	[#B	N�B	I�B	G�B	B�B	33B	/B	+B	+B	)�B	+B	)�B	&�B	$�B	"�B	!�B	�B	�B	�B	�B	{B	1B��B��B�B�B�`B�ZB�ZB�TB�;B�#B�B��BȴBǮBŢBB�}B�wB�qB�jB�^B�LB�9B�-B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B�B�B�9B�9B�FB�RB�LB�LB�^B�^B�RB�-B�B��B��B��B��B�!B�RB�dB�dB�RB�}B��B��B�B�#B�#B�5B�)B�BB�BB�`B�NB�HB�fB�B�B�B�yB�`B�;B�B�NB�NB�B�B��B��B	
=B	
=B	JB	JB	VB	\B	\B	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	"�B	"�B	!�B	�B	�B	�B	�B	�B	�B	%�B	'�B	&�B	$�B	%�B	&�B	)�B	,B	,B	/B	33B	7LB	5?B	49B	.B	/B	/B	(�B	+B	0!B	2-B	49B	2-B	D�B	@�B	2-B	#�B	�B	�B	�B	�B	�B	�B	 �B	"�B	'�B	-B	1'B	49B	7LB	:^B	<jB	=qB	>wB	@�B	B�B	E�B	E�B	G�B	H�B	I�B	L�B	N�B	P�B	Q�B	R�B	XB	[#B	_;B	aHB	aHB	aHB	bNB	cTB	cTB	dZB	dZB	e`B	e`B	gmB	l�B	o�B	p�B	s�B	t�B	t�B	u�B	v�B	w�B	z�B	~�B	�B	�1B	�7B	�=B	�VB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�?B	�RB	�dB	�dB	�jB	�}B	��B	��B	��B	��B	B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�HB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
	7B
oB
�B
�B
#�B
+B
2-B
8RB
@�B
E�B
J�B
P�B
VB
\)B
`BB
e`B
jB
o�B
t�B
x�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BD�BE�BD�BD�BD�BD�BD�BD�BD�BD�BD�BE�BE�BE�BE�BG�BG�BG�BI�BK�BL�BUB�FB�\B��B3B#�BJ�BO�BQ�BUBR�BQ�BR�BR�BR�BQ�BQ�BP�BYBVBR�BWBZ%B`LB^;Bi�Bv�B�SB�sB�PB�6B�B�B��B��B{�BQ�BA�B*B�BHB��B��B�$BǯB��B��B��B��B�vB�zB�VB��B��B�/Bk�B[(BM�B<jB%�B�BUB��B�B�NB�B��B�~B�B��B�BhvBUBC�B7MB&�BcB
@B
��B
�B
�SB
�?B
� B
��B
�B
� B
t�B
_CB
I�B
7SB
27B
.B
�B
�B	��B	�jB	�&B	�B	��B	�wB	�;B	�B	�!B	�B	��B	�B	u�B	g|B	[4B	N�B	I�B	G�B	B�B	3CB	/,B	+B	+B	*B	+B	*B	&�B	$�B	"�B	!�B	�B	�B	�B	�B	�B	DB�B��B�B�B�tB�nB�qB�jB�PB�7B�B��B��B��BŹB£B��B��B��B��B�sB�bB�NB�CB�8B�4B�*B�$B�B�B��B��B��B��B��B��B��B��B�B��B�B�B�B�B�B�B�B�B�B�)B�*B�NB�NB�\B�gB�bB�`B�sB�qB�jB�CB�0B�B��B��B��B�5B�gB�zB�yB�fB��B��B�B�*B�6B�4B�IB�>B�WB�VB�tB�cB�\B�zB��B�B�B�B�tB�QB�2B�cB�`B��B�B��B� B	
NB	
PB	[B	[B	gB	oB	lB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	"�B	"�B	!�B	�B	�B	�B	�B	�B	�B	%�B	'�B	&�B	$�B	%�B	&�B	*
B	,B	,B	/-B	3AB	7ZB	5OB	4HB	.#B	/*B	/*B	)B	+B	02B	2?B	4IB	2=B	D�B	@�B	2>B	#�B	�B	�B	�B	�B	�B	�B	 �B	"�B	'�B	-B	16B	4GB	7YB	:lB	<yB	=�B	>�B	@�B	B�B	E�B	E�B	G�B	H�B	I�B	L�B	N�B	P�B	Q�B	SB	XB	[0B	_LB	aTB	aUB	aVB	b\B	caB	caB	diB	dhB	emB	eoB	gzB	l�B	o�B	p�B	s�B	t�B	t�B	u�B	v�B	w�B	z�B	B	�B	�?B	�DB	�IB	�cB	�tB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�	B	�B	�B	�4B	�9B	�IB	�]B	�qB	�pB	�uB	��B	��B	��B	��B	��B	B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	� B	�'B	� B	�#B	� B	�.B	�1B	�?B	�FB	�KB	�RB	�_B	�_B	�^B	�bB	�cB	�iB	�iB	�pB	�vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
 B
 	B
 B
 B
 	B
B
 	B
 B
 	B
B
B
B
B
B
B
B
%B
"B
(B
'B
%B
'B
'B
&B
-B
.B
6B
4B
5B
:B
:B
;B
	>B
xB
�B
�B
#�B
+B
26B
8ZB
@�B
E�B
J�B
P�B
VB
\1B
`FB
eiB
j�B
o�B
t�B
x�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214392016053112143920160531121439  AO  ARCAADJP                                                                    20140930050138    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140930050138  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140930050138  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121439  IP                  G�O�G�O�G�O�                
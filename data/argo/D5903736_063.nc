CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:39Z AOML 3.0 creation; 2016-05-31T19:14:35Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20140721230539  20160531121435  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ?A   AO  4051_7090_063                   2C  D   APEX                            5368                            041511                          846 @���X-�1   @����F��@4�z�G��e"I�^51   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ?A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dys3D�fD�I�D�s3D��fD��D�6fD��3D��fD�3D�33D���D��fD�3D�L�DږfD��3D�3D�9�D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
BQ�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D%D%~�D%��D&�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dtk�Dyq�D��D�H�D�r�D���D�)D�5�D���D���D��D�2�D���D���D��D�L)Dڕ�D�ҏD��D�8�D�x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AС�AП�AЧ�AжFAв-AЮAЩ�AЬAЬAС�A�\)A�A�A�(�A�oA�A���A���A��
AρA�7LA���A� �A�~�A�&�A��A���A̶FA�hsA���A�ZA�&�Aʡ�A�-A�%A��;Aɧ�A�XA�7LA�$�A�|�A�A��A��A��A���A��#A�XA��TA��A�x�A��
A���A���A���A�E�A���A�G�A��A��hA��7A��A�  A�;dA�(�A�~�A���A�JA�x�A�ƨA�^5A��A�v�A��A���A�|�A�&�A��uA�M�A��
A��#A��\A��A�ZA�1A�K�A��`A�\)A���A��jA���A���A�7LA��mA�VA���A�+A���A���A���A���A�/A���A��A���A���A�oA�ffA�  A�A��A���A�p�A��
A���A�ƨA�`BA��A�l�A�+A���A��jA���AO�A}�
A{�FAy�#Ax�Aw��Av��AuƨAsƨArVApȴAnA�Ak�7Ai�7Ag�PAe�TAc��Aa��A_�wA^�A^�DA]��A\9XAZ �AX1AW+AV^5AT~�AO�-ALr�AHA�AE��AB�+AA�mA@�+A@JA>ĜA=��A<�RA:1'A9��A9\)A7��A5��A5
=A3�#A25?A1+A0�jA0~�A01'A/��A/C�A/&�A/�A.�/A.Q�A-��A,��A+�mA*�A*5?A*A)�TA(�!A%C�A#7LA ��A��A�AI�A��A�\A��A5?A��A?}A�A�A��Az�AS�A�A7LAbNA�jA��Ar�A/A	l�A��A&�A�DA�mA��AdZAr�A��A�yAJ@���@�V@��F@�dZ@�o@��\@�-@�@���@�33@��D@�t�@�;d@���@���@�E�@�@�/@�Q�@�^5@�?}@��@�r�@�1@�dZ@ݲ-@���@�ȴ@���@���@��@�=q@�bN@��#@��@���@�l�@�{@�x�@ȓu@��y@��@��@öF@°!@��T@�`B@��`@���@��@�t�@�o@���@�M�@��^@�x�@�p�@��D@�+@��@�O�@�V@��u@��m@�\)@�+@�
=@��@�o@��@�ȴ@�ȴ@��R@��H@�o@�C�@�S�@�+@�-@��j@��
@��F@��@���@�"�@�-@���@�V@��`@��j@��D@��@�r�@�I�@�1@���@���@�J@���@�G�@�&�@���@�Ĝ@��@�1@��y@��@�@��T@���@�X@�G�@��j@�I�@� �@�b@���@��w@�dZ@���@�{@��@��^@�p�@�I�@�l�@���@�^5@��@��^@���@���@���@���@���@�x�@�`B@�O�@�G�@��`@��@�Z@�Q�@�I�@��@��
@�ƨ@��w@�t�@�ȴ@�5?@��T@��^@��-@��@�X@��@�Ĝ@�z�@�  @��m@��;@��P@�K�@�+@��@�-@���@�X@��@���@��@��@��9@��@��D@��w@��P@�t�@�S�@�+@��@�@��@�ȴ@��!@���@�n�@�=q@�$�@�@��^@�hs@�G�@��@�r�@�1'@�1@�  @���@���@�"�@��@��@��@��@��y@���@�ff@�V@�V@�$�@��#@�@�@���@�p�@�`B@�O�@��@��@���@���@�Z@��@��@��
@��
@��
@��
@���@��F@�;d@���@�5?@���@�x�@�X@�%@��D@�I�@�  @��@�|�@�\)@�C�@�"�@�o@��@�@��7@��@�x�@�p�@�p�@���@��u@�r�@�I�@�1@���@�;d@�o@���@�~�@�V@�dZ@}��@w��@kt�@ahs@X�@R�H@N�y@C�
@<1@5@.�+@,(�@&�y@"�@�;@ƨ@�+@x�@�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AС�AП�AЧ�AжFAв-AЮAЩ�AЬAЬAС�A�\)A�A�A�(�A�oA�A���A���A��
AρA�7LA���A� �A�~�A�&�A��A���A̶FA�hsA���A�ZA�&�Aʡ�A�-A�%A��;Aɧ�A�XA�7LA�$�A�|�A�A��A��A��A���A��#A�XA��TA��A�x�A��
A���A���A���A�E�A���A�G�A��A��hA��7A��A�  A�;dA�(�A�~�A���A�JA�x�A�ƨA�^5A��A�v�A��A���A�|�A�&�A��uA�M�A��
A��#A��\A��A�ZA�1A�K�A��`A�\)A���A��jA���A���A�7LA��mA�VA���A�+A���A���A���A���A�/A���A��A���A���A�oA�ffA�  A�A��A���A�p�A��
A���A�ƨA�`BA��A�l�A�+A���A��jA���AO�A}�
A{�FAy�#Ax�Aw��Av��AuƨAsƨArVApȴAnA�Ak�7Ai�7Ag�PAe�TAc��Aa��A_�wA^�A^�DA]��A\9XAZ �AX1AW+AV^5AT~�AO�-ALr�AHA�AE��AB�+AA�mA@�+A@JA>ĜA=��A<�RA:1'A9��A9\)A7��A5��A5
=A3�#A25?A1+A0�jA0~�A01'A/��A/C�A/&�A/�A.�/A.Q�A-��A,��A+�mA*�A*5?A*A)�TA(�!A%C�A#7LA ��A��A�AI�A��A�\A��A5?A��A?}A�A�A��Az�AS�A�A7LAbNA�jA��Ar�A/A	l�A��A&�A�DA�mA��AdZAr�A��A�yAJ@���@�V@��F@�dZ@�o@��\@�-@�@���@�33@��D@�t�@�;d@���@���@�E�@�@�/@�Q�@�^5@�?}@��@�r�@�1@�dZ@ݲ-@���@�ȴ@���@���@��@�=q@�bN@��#@��@���@�l�@�{@�x�@ȓu@��y@��@��@öF@°!@��T@�`B@��`@���@��@�t�@�o@���@�M�@��^@�x�@�p�@��D@�+@��@�O�@�V@��u@��m@�\)@�+@�
=@��@�o@��@�ȴ@�ȴ@��R@��H@�o@�C�@�S�@�+@�-@��j@��
@��F@��@���@�"�@�-@���@�V@��`@��j@��D@��@�r�@�I�@�1@���@���@�J@���@�G�@�&�@���@�Ĝ@��@�1@��y@��@�@��T@���@�X@�G�@��j@�I�@� �@�b@���@��w@�dZ@���@�{@��@��^@�p�@�I�@�l�@���@�^5@��@��^@���@���@���@���@���@�x�@�`B@�O�@�G�@��`@��@�Z@�Q�@�I�@��@��
@�ƨ@��w@�t�@�ȴ@�5?@��T@��^@��-@��@�X@��@�Ĝ@�z�@�  @��m@��;@��P@�K�@�+@��@�-@���@�X@��@���@��@��@��9@��@��D@��w@��P@�t�@�S�@�+@��@�@��@�ȴ@��!@���@�n�@�=q@�$�@�@��^@�hs@�G�@��@�r�@�1'@�1@�  @���@���@�"�@��@��@��@��@��y@���@�ff@�V@�V@�$�@��#@�@�@���@�p�@�`B@�O�@��@��@���@���@�Z@��@��@��
@��
@��
@��
@���@��F@�;d@���@�5?@���@�x�@�X@�%@��D@�I�@�  @��@�|�@�\)@�C�@�"�@�o@��@�@��7@��@�x�@�p�@�p�@���@��u@�r�@�I�@�1@���@�;d@�o@���@�~�@�V@�dZ@}��@w��@kt�@ahs@X�@R�H@N�y@C�
@<1@5@.�+@,(�@&�y@"�@�;@ƨ@�+@x�@�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�9B�FB�FB�XB��B��B��B�B�
B�B�B�#B�)B�5B�NB�B��B��B��B��B�B�B�B�B�B�B�B��B  BB  B��B��B�B�B�
B�qB�-B��B�7B� Bw�BdZBYBK�BA�B>wB>wB=qB=qB<jB;dB8RB49B/B#�B�BDB��B��B��B�B�5B�dB��B��B��B��B�hB�JB�Bo�BgmBW
BB�B33B-B'�B�B�BDB��B�5B��B�}B�'B��B�uB�1B{�BjB[#BS�BB�B-B"�B�B%B
��B
�B
�BB
�B
��B
��B
ÖB
�}B
�FB
�B
��B
��B
�\B
�%B
t�B
e`B
aHB
^5B
Q�B
G�B
;dB
1'B
&�B
"�B
�B
�B
DB
B	��B	�yB	�B	��B	��B	�FB	�B	��B	��B	�uB	�hB	�DB	�B	t�B	iyB	bNB	]/B	L�B	'�B	{B	\B		7B��B��B�B�sB�`B�NB�BB�NB�fB�B�B�ZB�NB�#B��B��B��B��BȴBƨBŢBĜBĜBĜBBBB��B�wB�jB�dB�XB�3B��B��B��B��B�{B�oB�\B�PB�JB�=B�7B�1B�%B�B�B�B�B� B}�By�Bt�Bq�Bo�Bm�BiyBgmBffBe`BdZBdZBbNBbNBaHB_;B]/B^5B`BBaHB`BB`BBaHB`BB_;B_;BaHBcTBdZBdZBdZBcTBcTB`BBcTBgmBiyBiyBiyBiyBhsBgmBhsBhsBiyBhsBjBiyBiyBiyBk�Bo�Bs�Bv�Bv�Bw�By�B~�B�%B�7B�7B�DB�PB�VB�oB��B��B��B��B��B��B��B��B��B�B�9B�dBÖBǮB��B��B��B��B��B��B��B�
B�#B�)B�HB�TB�`B�mB�fB�B�B��B��B��B��B��B��B	B	B		7B	
=B	DB	DB	PB	VB	\B	\B	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	!�B	"�B	"�B	#�B	$�B	$�B	$�B	$�B	%�B	&�B	(�B	,B	,B	,B	-B	49B	:^B	C�B	H�B	L�B	O�B	Q�B	R�B	Q�B	R�B	T�B	W
B	XB	XB	XB	^5B	aHB	dZB	dZB	e`B	gmB	jB	k�B	k�B	n�B	s�B	x�B	{�B	|�B	|�B	~�B	� B	�B	�B	�B	�+B	�1B	�1B	�=B	�DB	�JB	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�9B	�FB	�FB	�LB	�LB	�XB	�dB	�jB	�qB	��B	ÖB	ĜB	ÖB	ĜB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�BB	�BB	�BB	�BB	�HB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
DB
{B
"�B
-B
49B
9XB
<jB
C�B
J�B
O�B
W
B
XB
]/B
aHB
dZB
hsB
m�B
q�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�?B�PB�PB�`B��B��B�B�B�B�B�'B�/B�3B�AB�VB�B��B��B��B��B��B�B�B�B�B�B��B��B 
B,B B��B��B�B�B�B�zB�8B��B�>B�Bw�BdbBYBK�BA�B>{B>zB=wB=zB<qB;jB8WB4>B/ B#�B�BHB��B��B��B�B�9B�eB��B��B��B��B�jB�LB�Bo�BgmBWBB�B38B-B'�B�B�BDB��B�6B��B�~B�(B��B�wB�5B{�Bj�B[&BS�BB�B-B"�B�B)B
��B
�B
�IB
�"B
�B
��B
ÜB
��B
�LB
�B
��B
��B
�dB
�*B
t�B
efB
aNB
^=B
Q�B
G�B
;nB
1/B
&�B
"�B
�B
�B
NB
B	��B	�B	�B	��B	��B	�QB	�B	��B	��B	��B	�tB	�SB	�B	t�B	i�B	b\B	]>B	L�B	(B	�B	mB		KB��B��B�B�B�tB�eB�UB�dB�xB�B�B�pB�aB�5B�B��B��B��B��BƼBŷBĲBĳBĳB¤B¦B£B��B��B��B�yB�nB�IB�B��B��B��B��B��B�uB�gB�`B�TB�OB�HB�=B�7B�/B�)B�%B�B~By�Bt�Bq�Bo�Bm�Bi�Bg�Bf�BezBdrBdtBbgBbgBacB_VB]EB^OB`[BabB`[B`]BacB`\B_SB_UBadBcoBdqBdrBdqBcmBcnB`\BcpBg�Bi�Bi�Bi�Bi�Bh�Bg�Bh�Bh�Bi�Bh�Bj�Bi�Bi�Bi�Bk�Bo�Bs�Bv�Bv�Bw�By�BB�:B�NB�PB�[B�fB�mB��B��B��B��B��B��B��B��B��B��B�B�OB�xBëB��B��B��B��B��B��B��B�B�B�5B�>B�[B�gB�qB�B�wB�B�B��B��B��B��B��B�B	B	)B		IB	
NB	VB	WB	aB	hB	mB	nB	sB	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	!�B	"�B	"�B	#�B	$�B	$�B	$�B	$�B	%�B	&�B	)B	,B	,B	,B	-!B	4GB	:nB	C�B	H�B	L�B	O�B	Q�B	S B	Q�B	S B	U
B	WB	XB	XB	XB	^CB	aWB	diB	dhB	elB	g}B	j�B	k�B	k�B	n�B	s�B	x�B	{�B	|�B	|�B	B	�B	�B	� B	�*B	�7B	�=B	�>B	�KB	�RB	�UB	�_B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�1B	�AB	�FB	�EB	�RB	�PB	�XB	�XB	�dB	�pB	�uB	�|B	��B	ÞB	ĩB	àB	ħB	ŬB	ƴB	ȽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�B	�B	�B	�B	�&B	�-B	�1B	�BB	�GB	�KB	�MB	�LB	�KB	�LB	�RB	�^B	�bB	�hB	�iB	�pB	�oB	�uB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B
JB
�B
"�B
-B
4AB
9_B
<oB
C�B
J�B
O�B
WB
XB
]3B
aNB
d_B
hyB
m�B
q�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214352016053112143520160531121435  AO  ARCAADJP                                                                    20140721230539    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230539  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230539  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121435  IP                  G�O�G�O�G�O�                
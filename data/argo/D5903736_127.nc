CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-14T09:15:40Z AOML 3.0 creation; 2016-05-31T19:14:45Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20151014091540  20160531121445  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_127                   2C  D   APEX                            5368                            041511                          846 @�v�����1   @�v�1�b?@3Η�O�;�dt�t�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�ffB�  B���B���B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dy� D�	�D�<�D���D���D�3D�9�D�p D��3D�	�D�9�D�ffDǙ�D� D�33Dڌ�D���D�fD�6fD�3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�BxQ�B�\)B���B�B�B���B���B���B�(�B���B�B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE�GCG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds�Dy��D��D�<)D���D��)D��D�8�D�o\D�D��D�8�D�e�Dǘ�D�\D�2�Dڌ)D��)D��D�5�D�D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�~�A�p�A�dZA�XA�XA�ZA�VA�=qA�/A�/A� �A���A���A��A��mA���Aް!AޮAޡ�Aޕ�AޑhAޏ\A�l�A�(�A��A݃A�33A�ȴAܟ�A�n�A�oA���A۝�A��#A�t�Aִ9A�VA�7LA�hsAиRA���A��A�(�A�`BA��#AŁAĥ�Aç�A�A�l�A�O�A�I�A���A�1'A��\A��A�&�A��7A�t�A�G�A���A�7LA�  A�l�A���A��A���A���A��^A��mA��HA��A��7A�9XA� �A�p�A��HA��TA�1A�v�A��A���A�A�A�1A�z�A��!A���A���A�7LA�A��A��A��A�t�A�\)A���A�A�A�A���A�7LA�ƨA��9A�|�A�oA�{A�Q�A���A���A��;A��A���A��A�-A~Q�Az�Aw��AuƨAt�!At�As��Ar$�Aq��Aq
=Apv�AoAn�DAl��Ak\)AjbAi`BAh��Ag��AfZAc��Ab�A`�/A`bNA^z�A\r�AZ�AVr�AU�AS�7AQ�;APE�AMXAJ��AI��AI�AG|�AE�7AD��ACoAAl�A@ZA?hsA<��A;�FA:�RA8  A6��A5�^A4bA2�+A1"�A/�wA-��A*�/A'�PA'S�A'G�A&��A&$�A%��A$ĜA#ƨA#33A"�A"bNA"-A!hsA M�A�HAƨA��AAƨAl�Az�A��AC�A��A�;A�jA��A
=A�DA33AE�A�A"�A��AA1'A
�`A
�A
�HA
1A�
A��A�TAVA�wA\)A%A Q�@�"�@��@�%@��!@��#@�%@�o@���@�b@�R@��@@@��@�l�@�=q@�7L@�@�&�@��@�v�@�^@��@ޏ\@��`@ܛ�@ۮ@�{@�V@٩�@��@ؓu@�9X@�\)@���@�n�@�hs@��@��@с@�Ĝ@�r�@Ϯ@ΰ!@ͺ^@�G�@̋D@�b@˕�@�+@�v�@�7L@���@���@ȃ@�t�@�@�~�@�J@Ł@�bN@Õ�@°!@�E�@�$�@���@��`@�  @�\)@���@�@���@��F@�dZ@��@��@�`B@�&�@���@�Ĝ@��u@�Q�@� �@���@��@�C�@��H@�v�@��+@�$�@���@�7L@�7L@�hs@��/@��u@�r�@�Z@�Q�@�1'@��m@��w@��@�C�@��@�^5@���@��@�X@��@�1@���@�-@���@��j@��@��H@��y@�-@�hs@���@���@��D@�j@�I�@���@���@�bN@� �@���@��@���@���@��/@��@��P@�C�@�ȴ@���@�=q@��T@��7@�7L@�/@�V@��`@��u@�Q�@���@���@���@��@�
=@�ȴ@�J@��7@�G�@�`B@�hs@�&�@�V@��`@�9X@�  @�1'@�Z@��@��w@��P@�t�@���@���@��+@��H@��H@��+@��@�V@���@��`@��`@���@�r�@� �@��@���@�C�@��H@���@���@�v�@�^5@�n�@�~�@�@�@���@���@��h@�?}@���@���@�r�@�b@�  @�r�@��P@��@��R@���@��!@�ȴ@���@���@�~�@�n�@�v�@�E�@���@��\@�M�@�M�@�-@�$�@��@��@���@�hs@�%@�j@�
=@��@��@���@�M�@���@��@���@�A�@��@�Ĝ@���@�b@���@�33@��H@��R@�$�@��@���@�x�@�hs@�O�@�?}@�?}@�`B@���@�z�@�A�@�1@��
@��
@�b@��@�t�@�K�@�+@�;d@�;d@�33@���@��u@~E�@t(�@jn�@a7L@X�u@Q�@G��@A��@;�m@5�@/\)@)�^@%�@ Q�@M�@�R@��@�R@	�^@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�~�A�p�A�dZA�XA�XA�ZA�VA�=qA�/A�/A� �A���A���A��A��mA���Aް!AޮAޡ�Aޕ�AޑhAޏ\A�l�A�(�A��A݃A�33A�ȴAܟ�A�n�A�oA���A۝�A��#A�t�Aִ9A�VA�7LA�hsAиRA���A��A�(�A�`BA��#AŁAĥ�Aç�A�A�l�A�O�A�I�A���A�1'A��\A��A�&�A��7A�t�A�G�A���A�7LA�  A�l�A���A��A���A���A��^A��mA��HA��A��7A�9XA� �A�p�A��HA��TA�1A�v�A��A���A�A�A�1A�z�A��!A���A���A�7LA�A��A��A��A�t�A�\)A���A�A�A�A���A�7LA�ƨA��9A�|�A�oA�{A�Q�A���A���A��;A��A���A��A�-A~Q�Az�Aw��AuƨAt�!At�As��Ar$�Aq��Aq
=Apv�AoAn�DAl��Ak\)AjbAi`BAh��Ag��AfZAc��Ab�A`�/A`bNA^z�A\r�AZ�AVr�AU�AS�7AQ�;APE�AMXAJ��AI��AI�AG|�AE�7AD��ACoAAl�A@ZA?hsA<��A;�FA:�RA8  A6��A5�^A4bA2�+A1"�A/�wA-��A*�/A'�PA'S�A'G�A&��A&$�A%��A$ĜA#ƨA#33A"�A"bNA"-A!hsA M�A�HAƨA��AAƨAl�Az�A��AC�A��A�;A�jA��A
=A�DA33AE�A�A"�A��AA1'A
�`A
�A
�HA
1A�
A��A�TAVA�wA\)A%A Q�@�"�@��@�%@��!@��#@�%@�o@���@�b@�R@��@@@��@�l�@�=q@�7L@�@�&�@��@�v�@�^@��@ޏ\@��`@ܛ�@ۮ@�{@�V@٩�@��@ؓu@�9X@�\)@���@�n�@�hs@��@��@с@�Ĝ@�r�@Ϯ@ΰ!@ͺ^@�G�@̋D@�b@˕�@�+@�v�@�7L@���@���@ȃ@�t�@�@�~�@�J@Ł@�bN@Õ�@°!@�E�@�$�@���@��`@�  @�\)@���@�@���@��F@�dZ@��@��@�`B@�&�@���@�Ĝ@��u@�Q�@� �@���@��@�C�@��H@�v�@��+@�$�@���@�7L@�7L@�hs@��/@��u@�r�@�Z@�Q�@�1'@��m@��w@��@�C�@��@�^5@���@��@�X@��@�1@���@�-@���@��j@��@��H@��y@�-@�hs@���@���@��D@�j@�I�@���@���@�bN@� �@���@��@���@���@��/@��@��P@�C�@�ȴ@���@�=q@��T@��7@�7L@�/@�V@��`@��u@�Q�@���@���@���@��@�
=@�ȴ@�J@��7@�G�@�`B@�hs@�&�@�V@��`@�9X@�  @�1'@�Z@��@��w@��P@�t�@���@���@��+@��H@��H@��+@��@�V@���@��`@��`@���@�r�@� �@��@���@�C�@��H@���@���@�v�@�^5@�n�@�~�@�@�@���@���@��h@�?}@���@���@�r�@�b@�  @�r�@��P@��@��R@���@��!@�ȴ@���@���@�~�@�n�@�v�@�E�@���@��\@�M�@�M�@�-@�$�@��@��@���@�hs@�%@�j@�
=@��@��@���@�M�@���@��@���@�A�@��@�Ĝ@���@�b@���@�33@��H@��R@�$�@��@���@�x�@�hs@�O�@�?}@�?}@�`B@���@�z�@�A�@�1@��
@��
@�b@��@�t�@�K�@�+@�;d@�;d@�33@���@��u@~E�@t(�@jn�@a7L@X�u@Q�@G��@A��@;�m@5�@/\)@)�^@%�@ Q�@M�@�R@��@�R@	�^@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B?}Bl�Bl�Bl�Bm�Bo�Bn�Bp�Bp�Br�Bs�Bv�Bs�Bs�B|�B��B�BƨB��B�;B��BDB�B^5B	7B=qBF�BO�BP�BA�B<jBQ�B^5BcTBffBn�Bo�Bo�BaHBO�BS�BD�B2-B'�B �B�B{BVB��B�HBĜB��B��B�oBȴB�B��B��B�3B��B�B�B�Bn�BH�B"�B��B�
B��BƨB�XB��B��B�-B�{Bu�Bs�B�B��B��B�B�B�wB�XB��B�7Bz�BjBS�B@�B5?B33B.B#�B�B\BPB
��B
�B
�/B
��B
�jB
�!B
��B
|�B
cTB
S�B
J�B
F�B
B�B
9XB
7LB
5?B
2-B
,B
#�B
�B

=B	��B	��B	�B	�`B	��B	�wB	�?B	�!B	�B	��B	��B	�%B	o�B	hsB	_;B	VB	L�B	=qB	1'B	+B	&�B	�B	�B	bB	1B	B��B�B�B�`B�HB�B�B��B��BƨB��B�dB�9B�B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B�{B�oB�\B�DB�B|�B|�B|�B� B�B�B� B~�B�B�%B�+B�JB�1B�DB�{B�VB|�B|�B{�Bk�BiyBhsBhsBhsBgmBgmBffBe`BdZBdZBhsBl�Bt�Bt�Bt�Bu�Bs�Bq�Bq�Bx�B}�B~�B~�B�B�B�B�B}�B� B�B�+B�PB�bB�{B��B��B��B��B��B��B��B��B�B�B�!B�'B�3B�FB�RB�RB�^B�^B�^B�XB�dB�wB�qB�qB�wB��BBĜBŢBǮB��B��B��B��B��B�B�B�)B�/B�5B�NB�fB�B�B�B��B��B��B��B��B��B��B��B	B	B	
=B	VB	hB	oB	oB	uB	�B	�B	�B	�B	#�B	$�B	%�B	%�B	%�B	&�B	)�B	+B	.B	2-B	6FB	:^B	;dB	;dB	=qB	D�B	H�B	G�B	F�B	F�B	J�B	O�B	W
B	ZB	\)B	bNB	hsB	iyB	iyB	iyB	n�B	r�B	t�B	u�B	w�B	z�B	|�B	}�B	}�B	�B	�B	�%B	�7B	�DB	�PB	�\B	�bB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�FB	�LB	�RB	�XB	�^B	�dB	�dB	�qB	�}B	��B	��B	�}B	�}B	�}B	�}B	�}B	�}B	�}B	�}B	�}B	��B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�;B	�HB	�NB	�ZB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
  B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
DB
�B
�B
'�B
/B
49B
:^B
>wB
D�B
I�B
O�B
W
B
\)B
dZB
iyB
n�B
q�B
v�B
x�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�"B?Bl�Bl�Bl�Bm�Bo�Bn�Bp�Bp�Br�Bs�Bv�Bs�Bs�B|�B��B�BƨB��B�=B��BEB�B^6B	6B=tBF�BO�BP�BA�B<mBQ�B^:Bc\BflBn�Bo�Bo�BaPBO�BS�BD�B22B'�B �B�B~B\B��B�JBĠB��B��B�uBȴB�B��B��B�4B��B�B�B�Bn�BH�B"�B��B�B��BƪB�YB��B��B�.B�~Bu�Bs�B�B��B��B�B�B�wB�ZB��B�;Bz�Bj�BS�B@�B5EB34B.B#�B�BaBVB
��B
�B
�3B
��B
�pB
�)B
��B
|�B
c]B
S�B
J�B
F�B
B�B
9aB
7UB
5GB
27B
,B
#�B
�B

EB	��B	��B	�B	�jB	��B	��B	�LB	�/B	�B	��B	��B	�2B	o�B	h�B	_KB	VB	L�B	=�B	19B	+B	&�B	�B	�B	tB	DB	B��B��B�B�tB�\B�2B�B��B��BƼB��B�zB�OB� B��B��B��B��B��B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B�sB�\B�0B}B}B}B�B�"B�B�BB�"B�=B�CB�aB�HB�[B��B�lB}B}B|Bk�Bi�Bh�Bh�Bh�Bg�Bg�BfBeyBdsBdsBh�Bl�Bt�Bt�Bt�Bu�Bs�Bq�Bq�Bx�B~
BBB�,B�1B�2B�&B~B�B�B�CB�iB�zB��B��B��B��B��B��B��B��B��B�B�#B�6B�;B�HB�ZB�gB�hB�tB�sB�rB�kB�wB��B��B��B��B��B§BįBŴB��B��B��B�B�B�B�B�*B�=B�AB�KB�aB�xB�B�B�B��B��B��B��B��B��B�B�B	 B	#B	
KB	hB	yB	�B	}B	�B	�B	�B	�B	�B	#�B	$�B	%�B	%�B	%�B	&�B	*B	+B	.%B	2;B	6UB	:oB	;uB	;vB	=�B	D�B	H�B	G�B	F�B	F�B	J�B	O�B	WB	Z.B	\;B	b\B	h�B	i�B	i�B	i�B	n�B	r�B	t�B	u�B	w�B	z�B	|�B	~B	~B	�B	�$B	�0B	�DB	�PB	�\B	�iB	�pB	�zB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�.B	�8B	�QB	�VB	�^B	�bB	�iB	�oB	�sB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	B	âB	ãB	ĩB	ŮB	ƲB	ȽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�!B	�'B	�+B	�CB	�SB	�VB	�eB	�jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
 B
B
 B
 B
 B	�B	��B
 B
B
"B
B
B
!B
(B
(B
)B
0B
MB
�B
�B
'�B
/$B
4AB
:eB
>B
D�B
I�B
O�B
WB
\.B
d^B
i�B
n�B
q�B
v�B
x�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214452016053112144520160531121445  AO  ARCAADJP                                                                    20151014091540    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151014091540  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151014091540  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121445  IP                  G�O�G�O�G�O�                
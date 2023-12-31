CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:15Z AOML 3.0 creation; 2016-05-31T19:14:27Z UW 3.1 conversion     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230515  20160531121427  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_018                   2C  D   APEX                            5368                            041511                          846 @�_�d��1   @�_��� @3���l�D�da�E��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� DyY�D�fD�@ D��3D���D�3D�C3D��fD�� D�3D�L�D���D�� D�3D�C3Dڜ�D���D� D�<�D�c3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
AУ�A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C{C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C`{Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��DxRD�RD~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D&D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE�RDF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��DyXRD��D�?\D���D��)D��D�B�D���D��\D��D�L)D��)D��\D��D�B�Dڜ)D��)D�\D�<)D�b�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�`BA�\)A�/A���A�v�A�t�A�n�A�ffA�\)A�\)A�XA�Q�A�O�A�I�A�E�A�E�A�A�A�9XA�(�A��A�VA��/A�A�JA�dZAԃAӁA�C�A��
Aҩ�AҁA�Q�A���A���Aч+A�(�A�C�Aϥ�A�-AΛ�A�jA�r�A�p�A��A�dZA���A�%A�hsA�C�A�r�A�9XA�G�A���A�7LA��!A�l�A�;dA�  A�&�A�z�A�33A�bNA���A�^5A�C�A�A��A���A���A�v�A���A�bNA�Q�A��A�jA�l�A���A�{A��FA�ffA��A�$�A���A�l�A�S�A��A�VA�C�A�
=A�|�A�A���A���A�&�A�(�A��A���A��^A��-A�~�A���A�n�A���A��\A|�A|�A{`BAzȴAxAtAp�uAo?}An�!AnbAlz�AjVAi
=Ae|�Aax�A`��A`M�A`5?A_�mA]��A\VA[�FAZȴAX�RAVVAS�#AQ�TAQ;dAO�^AOS�AO/AN��AN��ANbAK�#AK%AJVAI��AH�AF�+AD�+AC�FAB��A@�9A?"�A>VA=
=A;hsA8�A7S�A6�9A5�A4I�A2bNA1%A0�DA0=qA/�A/"�A.jA-dZA-�A,JA*=qA)`BA(��A(�A'K�A%dZA$=qA#��A"��A"��A!�A!oA �A��A
=A�A�7A�yA(�Ap�A�A�jAZA�PA�9A~�AM�A�-AffA$�A�AM�A�A��A=qA
=Az�A��A�A5?AA
r�A	�hA��A��Av�Al�A�jA��AI�A �@���@��@��@�|�@�ȴ@��j@��!@���@�dZ@�V@�O�@�@�Z@�  @�P@��^@�K�@�5?@�x�@���@�(�@��T@���@�Z@ݑh@ڸR@�x�@�@�@ՙ�@�x�@�%@ԓu@� �@�^5@��@�dZ@�;d@��@�J@̃@�5?@�M�@���@�b@��y@�(�@��`@�5?@���@�O�@�/@��9@��!@��@��!@��@���@��F@�ȴ@�J@�?}@�j@��w@���@�~�@�M�@���@���@�x�@��@��y@��@��^@�%@��u@�j@��@��@�"�@�M�@��h@�/@�%@��/@�Q�@��@�\)@���@���@�@���@���@��!@��\@��\@�M�@��T@�O�@��`@��9@�1'@�l�@�
=@��y@��H@���@��R@�ff@�@�`B@�&�@��@�%@��`@���@��@�Q�@�1'@��@��m@��w@��@�+@���@���@���@��+@�ff@��^@�7L@���@���@��D@�(�@�  @�ƨ@��@�@�~�@�$�@���@�`B@�?}@�&�@�%@���@��/@��9@�z�@�9X@�b@���@��P@��y@��R@�ff@�J@��^@���@��h@�O�@��@�z�@�j@�A�@�1@���@�\)@�C�@�o@��H@��\@�=q@�$�@���@���@��7@�x�@�p�@�G�@��@���@�Ĝ@��D@���@���@���@��@��F@��F@��F@��F@��F@���@�dZ@�+@���@���@��^@���@��7@�7L@��/@���@�r�@�Z@�Q�@��@��
@�ƨ@���@��@�|�@�+@���@���@��+@��@��@���@�G�@��@�Ĝ@��9@��9@���@�bN@��
@���@�t�@�
=@�-@�J@�@���@���@���@�7L@��@��@��
@��@��P@�K�@�33@���@��@���@�ȴ@���@��R@�v�@�-@�@�@�X@�V@��@��`@��/@��9@�j@��
@���@�|�@��y@�ff@�M�@�{@��j@x  @n@b^5@X��@PQ�@Fȴ@A%@:�@8  @2�\@-`B@(  @"-@p�@��@t�@��@��@	��@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�\)A�`BA�\)A�/A���A�v�A�t�A�n�A�ffA�\)A�\)A�XA�Q�A�O�A�I�A�E�A�E�A�A�A�9XA�(�A��A�VA��/A�A�JA�dZAԃAӁA�C�A��
Aҩ�AҁA�Q�A���A���Aч+A�(�A�C�Aϥ�A�-AΛ�A�jA�r�A�p�A��A�dZA���A�%A�hsA�C�A�r�A�9XA�G�A���A�7LA��!A�l�A�;dA�  A�&�A�z�A�33A�bNA���A�^5A�C�A�A��A���A���A�v�A���A�bNA�Q�A��A�jA�l�A���A�{A��FA�ffA��A�$�A���A�l�A�S�A��A�VA�C�A�
=A�|�A�A���A���A�&�A�(�A��A���A��^A��-A�~�A���A�n�A���A��\A|�A|�A{`BAzȴAxAtAp�uAo?}An�!AnbAlz�AjVAi
=Ae|�Aax�A`��A`M�A`5?A_�mA]��A\VA[�FAZȴAX�RAVVAS�#AQ�TAQ;dAO�^AOS�AO/AN��AN��ANbAK�#AK%AJVAI��AH�AF�+AD�+AC�FAB��A@�9A?"�A>VA=
=A;hsA8�A7S�A6�9A5�A4I�A2bNA1%A0�DA0=qA/�A/"�A.jA-dZA-�A,JA*=qA)`BA(��A(�A'K�A%dZA$=qA#��A"��A"��A!�A!oA �A��A
=A�A�7A�yA(�Ap�A�A�jAZA�PA�9A~�AM�A�-AffA$�A�AM�A�A��A=qA
=Az�A��A�A5?AA
r�A	�hA��A��Av�Al�A�jA��AI�A �@���@��@��@�|�@�ȴ@��j@��!@���@�dZ@�V@�O�@�@�Z@�  @�P@��^@�K�@�5?@�x�@���@�(�@��T@���@�Z@ݑh@ڸR@�x�@�@�@ՙ�@�x�@�%@ԓu@� �@�^5@��@�dZ@�;d@��@�J@̃@�5?@�M�@���@�b@��y@�(�@��`@�5?@���@�O�@�/@��9@��!@��@��!@��@���@��F@�ȴ@�J@�?}@�j@��w@���@�~�@�M�@���@���@�x�@��@��y@��@��^@�%@��u@�j@��@��@�"�@�M�@��h@�/@�%@��/@�Q�@��@�\)@���@���@�@���@���@��!@��\@��\@�M�@��T@�O�@��`@��9@�1'@�l�@�
=@��y@��H@���@��R@�ff@�@�`B@�&�@��@�%@��`@���@��@�Q�@�1'@��@��m@��w@��@�+@���@���@���@��+@�ff@��^@�7L@���@���@��D@�(�@�  @�ƨ@��@�@�~�@�$�@���@�`B@�?}@�&�@�%@���@��/@��9@�z�@�9X@�b@���@��P@��y@��R@�ff@�J@��^@���@��h@�O�@��@�z�@�j@�A�@�1@���@�\)@�C�@�o@��H@��\@�=q@�$�@���@���@��7@�x�@�p�@�G�@��@���@�Ĝ@��D@���@���@���@��@��F@��F@��F@��F@��F@���@�dZ@�+@���@���@��^@���@��7@�7L@��/@���@�r�@�Z@�Q�@��@��
@�ƨ@���@��@�|�@�+@���@���@��+@��@��@���@�G�@��@�Ĝ@��9@��9@���@�bN@��
@���@�t�@�
=@�-@�J@�@���@���@���@�7L@��@��@��
@��@��P@�K�@�33@���@��@���@�ȴ@���@��R@�v�@�-@�@�@�X@�V@��@��`@��/@��9@�j@��
@���@�|�@��y@�ff@�M�G�O�@��j@x  @n@b^5@X��@PQ�@Fȴ@A%@:�@8  @2�\@-`B@(  @"-@p�@��@t�@��@��@	��@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�!B�!B�!B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B�qB1'B�uB�3BÖB��B��B��B��BɺBȴBƨBÖB��B�jB�3B�B��B��B�oBu�B_;BW
BP�BI�B?}B1'B#�B�BPBB��B�mB��BŢBÖB��B�dB�LB�3B�B��B��B�bB�bB��B�BŢB�yB�B�TB�HB�5B��B��B�FB�B��B�hBe`BD�B#�BoB  B�;B��B_;BA�B'�B
��B
�`B
��B
�XB
��B
�B
v�B
{�B
{�B
|�B
u�B
^5B
C�B
7LB
.B
"�B
�B
�B
+B	�B	�HB	�#B	�B	��B	��B	B	�RB	��B	��B	�{B	�oB	�bB	�JB	�B	� B	{�B	v�B	m�B	cTB	T�B	G�B	G�B	<jB	;dB	:^B	8RB	9XB	7LB	33B	2-B	.B	)�B	%�B	�B	�B	oB	hB	DB	B	  B��B�B�B�fB�TB�BB�5B�)B�B�
B�B��B��B��B��B��BƨBB�}B�qB�dB�RB�9B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB��B��B��B��B��B�oB�PB�+B�B�%B�+B�%B�%B�1B�1B�%B�B�7B�DB�7B�+B�%B�B�B�B�B�B�B�B� B� B� B~�B�B�B�B�B�B|�B{�B{�By�Bx�Bu�Bn�Bl�BgmB^5BW
BVBR�BR�BR�BR�BS�BS�BS�BT�BW
BYBYB[#B\)B\)B\)Bk�B~�B�%B�Bx�Bp�Br�Bs�Bv�Bz�B{�B{�B~�B�B�7B�7B�JB�VB�bB�oB��B��B��B��B�B�!B�9B�?B�XB�^BBǮB��B��B��B�#B�HB�mB�B��B��B��B	  B	B	1B	VB	oB	oB	oB	uB	{B	�B	�B	�B	�B	�B	&�B	,B	.B	2-B	9XB	>wB	@�B	A�B	B�B	B�B	E�B	J�B	N�B	R�B	S�B	T�B	VB	YB	[#B	]/B	^5B	_;B	aHB	bNB	dZB	hsB	jB	l�B	l�B	n�B	o�B	u�B	x�B	z�B	{�B	}�B	� B	�B	�B	�B	�%B	�7B	�JB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�9B	�FB	�LB	�XB	�jB	�wB	��B	��B	��B	B	B	ÖB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�)B	�5B	�5B	�5B	�BB	�HB	�TB	�ZB	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
	7B
	7B
	7B

=B
	7B

=B
DB
DB
JB
PB
VB
VB
\B
\B
\B
bB
hB
hB
hB
oB
uB
uB
�B
�B
�B
"�B
+B
-B
8RB
<jB
A�B
D�B
G�B
L�B
Q�B
ZB
^5B
dZB
jB
p�B
u�B
x�B
|�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�&B�&B�$B�B�B�B�B�
B�B�B�B�B�B�
B�B�B�B�B�B��B��B��B�tB1*B�yB�;BáB��B��B��B��B��BȾBưBàB��B�tB�>B�B��B��B�sBu�B_?BWBP�BI�B?�B1)B#�B�BSBB��B�rB��BŞB×B��B�eB�PB�6B�B��B��B�dB�dB��B�BŦB�}B�B�XB�JB�8B��B��B�LB�B��B�kBebBD�B#�BrB B�=B��B_>BA�B'�B
��B
�eB
�B
�]B
��B
�B
v�B
{�B
{�B
|�B
u�B
^=B
C�B
7SB
.B
"�B
�B
�B
7B	��B	�RB	�-B	�B	�B	��B	B	�`B	�	B	��B	��B	�}B	�oB	�XB	�,B	�B	{�B	v�B	m�B	ccB	UB	G�B	G�B	<|B	;wB	:nB	8eB	9gB	7^B	3DB	2=B	.&B	*B	%�B	�B	�B	�B	yB	WB	#B	 B��B��B�B�zB�jB�UB�HB�>B�#B�B�B�B�B��B��B��BƻB§B��B��B�zB�gB�OB�?B�6B�*B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�jB�CB�8B�=B�FB�=B�<B�KB�JB�<B�6B�LB�[B�NB�EB�<B�0B�7B�1B�*B�1B�$B�!B�B�B�BB�$B�)B�*B�*B�B}B{�B|By�Bx�Bu�Bn�Bl�Bg�B^NBW#BVBS
BSBSBS
BTBTBTBUBW%BY1BY.B[=B\BB\BB\ABk�BB�:B�+Bx�Bp�Br�Bs�Bv�Bz�B| B{�BB�5B�NB�NB�`B�kB�xB��B��B��B��B�B�B�7B�MB�UB�lB�uB¤BǿB��B�B�B�7B�ZB�B�B��B��B�B	 B	0B	EB	iB	B	B	B	�B	�B	�B	�B	�B	�B	�B	&�B	,B	.'B	2;B	9fB	>�B	@�B	A�B	B�B	B�B	E�B	J�B	N�B	S B	T	B	UB	VB	Y'B	[1B	]?B	^BB	_IB	aVB	b[B	dhB	h�B	j�B	l�B	l�B	n�B	o�B	u�B	x�B	z�B	{�B	~B	�B	�B	�!B	�%B	�0B	�BB	�WB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�.B	�8B	�8B	�DB	�OB	�VB	�bB	�uB	��B	��B	��B	��B	B	B	áB	ƲB	ǷB	ǹB	ȾB	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�	B	�	B	�B	�B	�B	�!B	�3B	�?B	�@B	�>B	�KB	�QB	�^B	�cB	�kB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B
 B
B
B
B
B
B
B
B
B
B
B
B
B
$B
4B
:B
	@B
	?B
	@B

GB
	?B

GB
NB
KB
QB
XB
\B
]B
dB
dB
cB
iB
qB
rB
pB
uB
}B
}G�O�B
�B
�B
"�B
+B
-B
8YB
<oB
A�B
D�B
G�B
L�B
Q�B
Z#B
^:B
d`B
j�B
p�B
u�B
x�B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214272016053112142720160531121427  AO  ARCAADJP                                                                    20140721230515    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230515  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230515  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121427  IP                  G�O�G�O�G�O�                
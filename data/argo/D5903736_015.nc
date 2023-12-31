CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:14Z AOML 3.0 creation; 2016-05-31T19:14:26Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230514  20160531121427  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_015                   2C  D   APEX                            5368                            041511                          846 @�W눈�1   @�W�&�  @4|�hs�d@�1&�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy��D�fD�33D��fD��3D���D�FfD�y�D��fD�fD�33D�vfD�� D�  D�FfDچfD�� D�	�D�I�D�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @1�@xQ�@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dtk�Dy�RD��D�2�D���D�D��)D�E�D�x�D���D��D�2�D�u�D��\D��\D�E�Dڅ�D��\D��D�H�D��D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AԮAԮA԰!A԰!AԲ-A԰!AԴ9AԴ9AԶFAԴ9AԴ9AԺ^AԼjA�A�A���A���A�A�A�ĜA�ȴA�ȴA���A���A���A���A���AԬAԋDA�hsA�G�A�  A�VA��mA��Aʇ+A�^5A�1'Aȡ�A�Q�Aư!A�~�A��;A�"�Aĩ�A���AîA�1'A�;dA�hsA�+A��A�5?A���A��+A�\)A�7LA��A�1A�7LA�
=A���A�;dA�JA�oA��A�+A��A��;A���A�+A�v�A���A�^5A��HA�t�A�bA�VA��A�^5A�hsA�XA���A�bA�ĜA���A�G�A�ƨA�C�A��TA��A�"�A��RA�-A�bA��-A�+A�hsA�l�A��
A��#A�VA��PA��yA��mA�  A�p�A��mA���A�+A���A�=qA��uA��-A���A��HA�33A���A��uA�G�A}�FA{?}Az��Az^5Ay�Ax�Aw�Av$�At  Ar��Ar(�AqG�Ao�mAm�#AlĜAi�TAf�9Ad�+Ac�Ac+AbȴAa�-A\��A[�-A[S�AZQ�AW��AUx�AS7LAP��AN�DAL5?AKl�AJ�AI�PAHE�AF��AE`BAB�A@�RA>ĜA<M�A;A:z�A:$�A9�A7�A5�PA4ĜA2 �A.z�A.=qA,�`A)��A'"�A&�A&r�A%�A$A�A#dZA"�\A"(�A!t�A!&�A 9XA�A  A/A9XAp�Az�A�uA�A�;A�A/A�Ar�A?}AK�A�jAbA�AƨA�`AhsAjA%A��A	;dA��AC�A9XA^5A33A��A�mA+@�V@�&�@�j@���@���@�+@�-@��/@�o@��@��H@�M�@��T@�7L@�9X@�b@�F@��@�@�5?@�"�@�@�(�@��T@�7L@���@�b@ߍP@�
=@���@�j@ڗ�@�1'@�
=@�X@� �@�ȴ@��T@�X@��@�5?@�\)@ʗ�@ɡ�@�o@þw@+@�=q@�@��@�O�@�/@��j@�9X@�;d@���@��@���@�hs@�Ĝ@��D@�(�@�|�@��F@��@�1'@�  @��P@��@��#@�O�@��@�\)@�@��H@��y@�~�@��y@�33@��w@�l�@�;d@�ff@�5?@��@��u@��w@�n�@��@���@��^@���@���@���@��@�&�@���@��@��;@���@�"�@�-@�@��@�x�@�{@�{@���@��@���@���@�A�@��m@�ƨ@�|�@�ȴ@�^5@�$�@��-@�%@�z�@�(�@���@�;d@���@�v�@�J@��-@��7@�X@�/@���@��D@�I�@�A�@�(�@��;@�|�@�dZ@�+@��H@���@���@�n�@�^5@�=q@�J@��@��T@���@���@��7@�hs@�O�@�&�@���@��/@��@��u@�j@�I�@��m@��P@�+@�@��y@�ȴ@���@�ff@�{@��T@��#@���@�G�@�/@�&�@���@���@�j@�9X@��@��w@��P@�t�@�\)@�+@��@���@�$�@���@���@�@�@���@�V@�Ĝ@��9@���@�Z@�b@���@��F@�dZ@�dZ@�\)@�\)@�33@��@��y@���@�{@��@���@��-@��h@�`B@�7L@���@��j@�Z@�1@�  @���@��
@��F@��@�C�@���@���@��+@�=q@�{@��#@��-@���@�O�@�V@��@��/@�Ĝ@��u@�j@��@��
@���@�K�@��y@���@�=q@�@���@��7@�7L@���@��@�Z@�Q�@�1'@��;@��@��@��P@���@��!@���@��\@�n�@�E�@�5?@��@��@�z�@��@��@x�`@o��@e?}@\�@T(�@Kt�@E��@<��@7
=@/;d@(��@"n�@��@�7@��@��@z�@	X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AԮAԮA԰!A԰!AԲ-A԰!AԴ9AԴ9AԶFAԴ9AԴ9AԺ^AԼjA�A�A���A���A�A�A�ĜA�ȴA�ȴA���A���A���A���A���AԬAԋDA�hsA�G�A�  A�VA��mA��Aʇ+A�^5A�1'Aȡ�A�Q�Aư!A�~�A��;A�"�Aĩ�A���AîA�1'A�;dA�hsA�+A��A�5?A���A��+A�\)A�7LA��A�1A�7LA�
=A���A�;dA�JA�oA��A�+A��A��;A���A�+A�v�A���A�^5A��HA�t�A�bA�VA��A�^5A�hsA�XA���A�bA�ĜA���A�G�A�ƨA�C�A��TA��A�"�A��RA�-A�bA��-A�+A�hsA�l�A��
A��#A�VA��PA��yA��mA�  A�p�A��mA���A�+A���A�=qA��uA��-A���A��HA�33A���A��uA�G�A}�FA{?}Az��Az^5Ay�Ax�Aw�Av$�At  Ar��Ar(�AqG�Ao�mAm�#AlĜAi�TAf�9Ad�+Ac�Ac+AbȴAa�-A\��A[�-A[S�AZQ�AW��AUx�AS7LAP��AN�DAL5?AKl�AJ�AI�PAHE�AF��AE`BAB�A@�RA>ĜA<M�A;A:z�A:$�A9�A7�A5�PA4ĜA2 �A.z�A.=qA,�`A)��A'"�A&�A&r�A%�A$A�A#dZA"�\A"(�A!t�A!&�A 9XA�A  A/A9XAp�Az�A�uA�A�;A�A/A�Ar�A?}AK�A�jAbA�AƨA�`AhsAjA%A��A	;dA��AC�A9XA^5A33A��A�mA+@�V@�&�@�j@���@���@�+@�-@��/@�o@��@��H@�M�@��T@�7L@�9X@�b@�F@��@�@�5?@�"�@�@�(�@��T@�7L@���@�b@ߍP@�
=@���@�j@ڗ�@�1'@�
=@�X@� �@�ȴ@��T@�X@��@�5?@�\)@ʗ�@ɡ�@�o@þw@+@�=q@�@��@�O�@�/@��j@�9X@�;d@���@��@���@�hs@�Ĝ@��D@�(�@�|�@��F@��@�1'@�  @��P@��@��#@�O�@��@�\)@�@��H@��y@�~�@��y@�33@��w@�l�@�;d@�ff@�5?@��@��u@��w@�n�@��@���@��^@���@���@���@��@�&�@���@��@��;@���@�"�@�-@�@��@�x�@�{@�{@���@��@���@���@�A�@��m@�ƨ@�|�@�ȴ@�^5@�$�@��-@�%@�z�@�(�@���@�;d@���@�v�@�J@��-@��7@�X@�/@���@��D@�I�@�A�@�(�@��;@�|�@�dZ@�+@��H@���@���@�n�@�^5@�=q@�J@��@��T@���@���@��7@�hs@�O�@�&�@���@��/@��@��u@�j@�I�@��m@��P@�+@�@��y@�ȴ@���@�ff@�{@��T@��#@���@�G�@�/@�&�@���@���@�j@�9X@��@��w@��P@�t�@�\)@�+@��@���@�$�@���@���@�@�@���@�V@�Ĝ@��9@���@�Z@�b@���@��F@�dZ@�dZ@�\)@�\)@�33@��@��y@���@�{@��@���@��-@��h@�`B@�7L@���@��j@�Z@�1@�  @���@��
@��F@��@�C�@���@���@��+@�=q@�{@��#@��-@���@�O�@�V@��@��/@�Ĝ@��u@�j@��@��
@���@�K�@��y@���@�=q@�@���@��7@�7L@���@��@�Z@�Q�@�1'@��;@��@��@��P@���@��!@���@��\@�n�@�E�@�5?@��@��@�z�@��@��@x�`@o��@e?}@\�@T(�@Kt�@E��@<��@7
=@/;d@(��@"n�@��@�7@��@��@z�@	X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB%�B%�B&�B&�B%�B&�B&�B&�B&�B&�B&�B&�B&�B%�B%�B%�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B$�B#�B"�B �B�B�B�B(�B33B8RB8RB6FB0!B.B-B+B"�B�B�B�B�B�B$�B.B;dBE�BO�BW
B[#B]/BaHBjBo�Bn�Bo�Bt�B{�B|�B�B�B~�Bp�Br�By�Br�BffBbNB]/BYB[#BR�BM�BH�BI�BO�BL�BF�B5?B1'B%�B1B�BB+B��B�sB�B��B��BŢB�}B�FB��B��B�hB�7B~�Bs�BiyBaHBVB<jB�BbB
��B
�mB
�B
��B
��B
�?B
�B
��B
x�B
e`B
VB
P�B
N�B
H�B
B�B
:^B
,B
�B
�B
bB
	7B	��B	�B	�yB	�B	ƨB	�^B	�?B	�'B	�B	��B	�PB	�+B	�B	{�B	o�B	cTB	W
B	K�B	C�B	;dB	8RB	49B	/B	(�B	 �B	�B	\B	+B��B��B�B�B�B�B�ZB�)B�BȴB�qB�dB�9B�B��B��B��B��B��B��B��B��B��B��B�uB�\B�JB�=B�7B�+B�B�B�B�B�B�B� B}�Bz�Bx�Bw�Bw�Bv�Bt�Br�Bo�Bl�BiyBe`BbNB_;B]/BZBXBW
BW
BT�BS�BQ�BQ�BP�BP�BP�BP�BP�BP�BW
B[#B_;BaHBbNB`BBaHBcTBhsBm�Bp�Bv�Br�Bm�Bn�Bt�B{�B{�B|�B}�B}�B� B� B�B�B�%B�B�B�B�B�B�Bz�Bw�Bt�Br�Bm�BffBdZBdZBdZBdZBe`Be`Be`BgmBw�B~�B�7B�1B�+B�7B�=B�JB�hB��B��B��B�B�!B�B�B��B��B��B�B�?B�XB�dB��BŢB��B��B��B�B�B�
B�
B�)B�TB�fB�mB�mB�mB�sB�sB�B�B�B�B�B��B��B��B��B	B	VB	�B	�B	�B	"�B	$�B	%�B	(�B	+B	,B	.B	2-B	49B	6FB	9XB	>wB	B�B	D�B	H�B	J�B	M�B	O�B	R�B	VB	XB	YB	ZB	\)B	^5B	`BB	`BB	aHB	bNB	ffB	hsB	iyB	l�B	l�B	m�B	q�B	q�B	r�B	s�B	t�B	u�B	u�B	v�B	w�B	w�B	x�B	y�B	z�B	{�B	|�B	}�B	~�B	� B	�B	�B	�1B	�7B	�=B	�DB	�JB	�VB	�hB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�9B	�9B	�9B	�9B	�FB	�RB	�RB	�RB	�XB	�dB	�qB	�wB	��B	��B	��B	��B	��B	B	ÖB	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�TB	�`B	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
1B
VB
uB
�B
&�B
/B
33B
9XB
?}B
C�B
K�B
O�B
T�B
ZB
aHB
ffB
jB
n�B
r�B
w�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B%�B%�B&�B&�B%�B&�B&�B&�B&�B&�B&�B&�B&�B%�B%�B%�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B$�B#�B"�B �B�B�B�B(�B32B8RB8RB6JB0$B.B-B+ B"�B�B�B�B�B�B$�B.B;eBE�BO�BWB['B]1BaGBj�Bo�Bn�Bo�Bt�B{�B|�B�	B�B~�Bp�Br�By�Br�BfiBbRB]1BYB[*BR�BM�BH�BI�BO�BL�BF�B5BB1(B%�B1B�BB*B��B�uB�B��B��BŢB�B�HB��B��B�iB�<B~�Bs�Bi{BaLBVB<lB�BeB
��B
�pB
�!B
��B
��B
�CB
�B
��B
x�B
egB
VB
P�B
N�B
H�B
B�B
:fB
,B
�B
�B
nB
	@B	�B	�B	�B	�#B	ƳB	�jB	�JB	�4B	�!B	��B	�]B	�7B	� B	{�B	o�B	ccB	WB	K�B	C�B	;wB	8bB	4JB	/,B	)B	 �B	�B	rB	>B�B��B�B�B��B�B�qB�?B�B��B��B�zB�OB�B��B��B��B��B��B��B��B��B��B��B��B�uB�_B�TB�PB�AB�5B�0B�*B�%B�%B�!B�B~Bz�Bx�Bw�Bw�Bv�Bt�Br�Bo�Bl�Bi�BexBbhB_UB]FBZ7BX*BW$BW$BUBTBRBRBQBQ BP�BQBQ BQ BW$B[;B_UBacBbfB`]BabBcoBh�Bm�Bp�Bv�Br�Bm�Bn�Bt�B| B| B}B~B~B�B�B� B�,B�<B�6B�2B�1B�1B�+B�"Bz�Bw�Bt�Br�Bm�BfBdrBdrBdqBdrBexBexBeyBg�Bw�BB�MB�IB�BB�OB�TB�aB��B��B��B��B�B�6B�+B�B�B�B�B�+B�VB�mB�vB��BŶB��B��B�B�B�,B�B�B�;B�hB�wB�B�~B�B�B�B�B��B�B��B��B��B��B��B��B	"B	hB	�B	�B	�B	"�B	$�B	%�B	)B	+B	,B	.$B	2>B	4HB	6UB	9hB	>�B	B�B	D�B	H�B	J�B	M�B	O�B	R�B	VB	XB	Y'B	Z-B	\8B	^CB	`OB	`OB	aWB	b[B	frB	h�B	i�B	l�B	l�B	m�B	q�B	q�B	r�B	s�B	t�B	u�B	u�B	v�B	w�B	w�B	x�B	y�B	z�B	{�B	|�B	~B	B	�B	�!B	�+B	�>B	�CB	�KB	�PB	�WB	�cB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�%B	�7B	�BB	�GB	�EB	�EB	�FB	�OB	�]B	�]B	�]B	�cB	�oB	�}B	��B	��B	��B	��B	��B	��B	B	àB	ĨB	ǻB	ȾB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�"B	�!B	�&B	�-B	�3B	�6B	�;B	�@B	�DB	�LB	�QB	�RB	�RB	�XB	�\B	�iB	�jB	�nB	�wB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
 	B
 	B
 
B
B
B
:B
^B
}B
�B
&�B
/#B
3:B
9bB
?�B
C�B
K�B
O�B
UB
Z#B
aKB
fkB
j�B
n�B
r�B
w�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214272016053112142720160531121427  AO  ARCAADJP                                                                    20140721230514    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230514  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230514  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121427  IP                  G�O�G�O�G�O�                
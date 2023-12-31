CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:53Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181121041153  20190604094023  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�Ն3�dz1   @�ՆǮ�@3
=p���d�$�/1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  BhffBnffBx  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D��)D�N�D���D��D�3D�L{D�o�D��)D�3D�@RD���D��D��D�FfD�nfD��D�HD�H D�mqD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @6fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B(33B/��B7��B?��BG��BO��BW��B_��Bh33Bn33Bw��B��B��fB��fB��fB��fB��B��fB��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Do3Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt�Dy�{D��D�MD��)D�ӅD��D�J�D�nD�ʏD�	�D�>�D��
D�˅D� �D�D�D�l�D�RD��D�FfD�k�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�\)A�ZA�dZA�bNA�`BA�E�A�?}A�A�A�5?A��A��A���AؾwAجAؙ�A؋DA�z�A�v�A�p�A�^5A�M�A�E�A�=qA�33A�-A�$�A�  AՓuA��A�&�Aɺ^A�S�A�5?A��Aã�A�VA�A�A���A�Q�A�"�A��A�bNA�bNA���A�33A�=qA���A�;dA���A�ffA���A�~�A���A�(�A�&�A�hsA��uA�{A�M�A�^5A�?}A��A��A�Q�A���A�E�A�p�A���A�G�A�{A�%A��A�A�A�/A�{A�ĜA���A�9XA�JA�ffA�JA��PA�I�A�n�A��9A�?}A���A�
=A�7LA�ĜA��HA�O�A�{A��A��TA�ƨA�ZA���A�A�A��A�{A|�9Ay�^Axz�AwK�Av-AtĜAt9XAsoAq�Ao/Am%Ak�Aj�\Ah�Af�DAdAc�A`�A]7LAZ��AX��AV=qAS��AR�RAQ��APE�AM�
AJ��AI�AH�!AG/AFffAEl�AD��AD1ACdZAB��AB  AA%A@VA?�#A?&�A<��A;��A9�
A7�FA7K�A5x�A1�A0A�A/�A/�;A/ƨA/t�A-�A+S�A*��A)�A&��A%x�A"�A"$�A!K�A�A��Az�AE�A�^A33A+A?}A7LA�HA/A��A�AG�A�+A�RAO�AbNA5?A��A��A��AȴAjA��A
��A
�/A
��A
(�A	�TA	�hA�A��A�9A$�A�A��A33AJAffAC�A bN@�$�@�bN@���@�^5@��h@�&�@��@�b@�|�@�J@���@홚@�9X@�(�@�@�x�@���@�9X@�33@��T@���@�z�@��@�/@�Ĝ@�1'@���@�I�@�|�@ް!@�&�@��@ڏ\@؛�@׾w@��@�$�@�?}@�-@Ь@ЋD@��m@��H@�{@͡�@̬@��
@�C�@�~�@ə�@ǶF@�M�@��@å�@°!@���@��/@�b@�l�@�33@���@��@�x�@��@���@�;d@��#@��j@�r�@�(�@��m@���@���@�{@���@��@�M�@�?}@�A�@���@���@�@�x�@��@��@�1@�K�@�;d@�C�@�o@�n�@��-@��@�G�@�/@�7L@�?}@�O�@�?}@��9@��!@�=q@�-@�J@��@��j@�b@�"�@�
=@��y@��\@���@���@���@��h@�?}@���@�9X@�ƨ@��@�dZ@�dZ@�;d@���@�v�@�=q@�G�@���@��9@��@�Z@�1'@��m@�t�@�+@�@��R@�5?@��@���@��^@��7@��@��@�r�@�Z@�  @��w@�\)@��@���@�M�@�@��T@��^@���@�p�@�O�@�O�@�G�@��`@���@���@��u@�bN@�b@�  @��@���@��F@���@��@�K�@�;d@�;d@�
=@���@�-@��@��#@���@��@�J@��#@���@���@���@��@��u@�1'@�r�@��@���@��@���@�bN@�bN@�  @���@���@���@��P@�S�@�33@�+@�
=@���@��R@�v�@�G�@��@��9@��9@��@��9@��9@�bN@��w@�dZ@�C�@�K�@���@�E�@�$�@��#@��-@��7@�X@�7L@��@��@��@�z�@�I�@�b@��
@��F@��@���@�K�@�"�@�@���@�$�@���@�`B@��@��@��D@�I�@���@�S�@�;d@�33@��@��@��H@���@�v�@�E�@��T@��h@�p�@�`B@�G�@�&�@�%@���@���@��@���@�Z@��@���@��F@��P@��@�t�@�l�@�S�@�@��!@��+@�Xy@w�
@qF@i�@^��@YS&@P��@H[�@B��@;o�@31�@+t�@&��@!2a@�@��@E�@�@m�@l"@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�dZA�\)A�ZA�dZA�bNA�`BA�E�A�?}A�A�A�5?A��A��A���AؾwAجAؙ�A؋DA�z�A�v�A�p�A�^5A�M�A�E�A�=qA�33A�-A�$�A�  AՓuA��A�&�Aɺ^A�S�A�5?A��Aã�A�VA�A�A���A�Q�A�"�A��A�bNA�bNA���A�33A�=qA���A�;dA���A�ffA���A�~�A���A�(�A�&�A�hsA��uA�{A�M�A�^5A�?}A��A��A�Q�A���A�E�A�p�A���A�G�A�{A�%A��A�A�A�/A�{A�ĜA���A�9XA�JA�ffA�JA��PA�I�A�n�A��9A�?}A���A�
=A�7LA�ĜA��HA�O�A�{A��A��TA�ƨA�ZA���A�A�A��A�{A|�9Ay�^Axz�AwK�Av-AtĜAt9XAsoAq�Ao/Am%Ak�Aj�\Ah�Af�DAdAc�A`�A]7LAZ��AX��AV=qAS��AR�RAQ��APE�AM�
AJ��AI�AH�!AG/AFffAEl�AD��AD1ACdZAB��AB  AA%A@VA?�#A?&�A<��A;��A9�
A7�FA7K�A5x�A1�A0A�A/�A/�;A/ƨA/t�A-�A+S�A*��A)�A&��A%x�A"�A"$�A!K�A�A��Az�AE�A�^A33A+A?}A7LA�HA/A��A�AG�A�+A�RAO�AbNA5?A��A��A��AȴAjA��A
��A
�/A
��A
(�A	�TA	�hA�A��A�9A$�A�A��A33AJAffAC�A bN@�$�@�bN@���@�^5@��h@�&�@��@�b@�|�@�J@���@홚@�9X@�(�@�@�x�@���@�9X@�33@��T@���@�z�@��@�/@�Ĝ@�1'@���@�I�@�|�@ް!@�&�@��@ڏ\@؛�@׾w@��@�$�@�?}@�-@Ь@ЋD@��m@��H@�{@͡�@̬@��
@�C�@�~�@ə�@ǶF@�M�@��@å�@°!@���@��/@�b@�l�@�33@���@��@�x�@��@���@�;d@��#@��j@�r�@�(�@��m@���@���@�{@���@��@�M�@�?}@�A�@���@���@�@�x�@��@��@�1@�K�@�;d@�C�@�o@�n�@��-@��@�G�@�/@�7L@�?}@�O�@�?}@��9@��!@�=q@�-@�J@��@��j@�b@�"�@�
=@��y@��\@���@���@���@��h@�?}@���@�9X@�ƨ@��@�dZ@�dZ@�;d@���@�v�@�=q@�G�@���@��9@��@�Z@�1'@��m@�t�@�+@�@��R@�5?@��@���@��^@��7@��@��@�r�@�Z@�  @��w@�\)@��@���@�M�@�@��T@��^@���@�p�@�O�@�O�@�G�@��`@���@���@��u@�bN@�b@�  @��@���@��F@���@��@�K�@�;d@�;d@�
=@���@�-@��@��#@���@��@�J@��#@���@���@���@��@��u@�1'@�r�@��@���@��@���@�bN@�bN@�  @���@���@���@��P@�S�@�33@�+@�
=@���@��R@�v�@�G�@��@��9@��9@��@��9@��9@�bN@��w@�dZ@�C�@�K�@���@�E�@�$�@��#@��-@��7@�X@�7L@��@��@��@�z�@�I�@�b@��
@��F@��@���@�K�@�"�@�@���@�$�@���@�`B@��@��@��D@�I�@���@�S�@�;d@�33@��@��@��H@���@�v�@�E�@��T@��h@�p�@�`B@�G�@�&�@�%@���@���@��@���@�Z@��@���@��F@��P@��@�t�@�l�@�S�@�@��!G�O�@�Xy@w�
@qF@i�@^��@YS&@P��@H[�@B��@;o�@31�@+t�@&��@!2a@�@��@E�@�@m�@l"@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
=BDBJB
=BDBDBbBoBhB{B�B"�B+B33B=qBJ�BQ�B\)BgmBo�Bv�Bw�Bw�Bw�Bw�Bw�Bw�Bv�B�B�B��B�B�B�sB��B��B��B��B��B��B��B��B�B�B�mB�TB�)B�
B��B��B��BĜB�qB�RB��B�{B�DB�B� B|�B�B{�B\)BVBR�BN�BH�BB�B;dB.B#�B"�B �B�BB��B��B�B�B�;B�
B��B��BǮB�dB�'B��B��B~�BhsBbNBS�B@�B/B�BVB
��B
�B
��B
�FB
��B
��B
� B
k�B
bNB
XB
M�B
C�B
?}B
8RB
.B
�B
bB
B	��B	�B	�/B	��B	��B	�FB	��B	�VB	|�B	k�B	\)B	VB	M�B	C�B	/B	�B	�B	{B	PB	
=B	%B	B	B��B��B��B��B�B�B�B�TB�5B�
B��B��BŢB�RB�?B�9B�3B�-B�B��B��B��B��B��B�hB�PB�DB�1B�B�B�B�B�%B�JB�bB�oB�hB�oB�oB�{B�uB�oB�VB�JB�7B�1B�+B�%B�B}�B|�B|�B~�B� B� B� B�B� B�B�B�B�B�B�B� B~�B}�By�B{�Bz�Bz�Bx�Bw�Bx�Bw�Bw�Bv�Bw�B~�B�B~�Bz�B{�B�+B�PB�VB�PB�JB�oB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�-B�RB�XB�^B�jB��BBĜBǮBȴBȴB��B��B��B��B��B��B��B�
B�B�/B�ZB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	%B	+B	1B	
=B	JB	uB	�B	�B	�B	�B	"�B	&�B	+B	7LB	;dB	<jB	=qB	A�B	A�B	G�B	H�B	H�B	H�B	L�B	O�B	O�B	N�B	YB	[#B	\)B	aHB	e`B	e`B	ffB	jB	n�B	q�B	r�B	t�B	u�B	v�B	x�B	}�B	� B	� B	�B	�B	�B	�+B	�7B	�DB	�PB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�FB	�RB	�XB	�dB	�jB	�jB	�qB	�}B	��B	B	B	ÖB	ĜB	ĜB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�BB	�NB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�ZB	�TB	�TB	�ZB	�`B	�`B	�`B	�ZB	�`B	�`B	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B

=B
DB
DB
DB

=B

=B

=B

=B
DB
PB
VB
VB
PB
\B
bB
hB
hB
hB
hB
oB
hB
hB
oB
uB
{B
YB
�B
'�B
/�B
9>B
=B
E�B
MjB
R�B
X�B
^B
dtB
h�B
l�B
pB
tB
zDB
HB
�uB
�YB
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 BIBMBXBFBMBMBmB|BrB�B�B�B'B/=B9{BF�BM�BX4BcvBk�Br�Bs�Bs�Bs�Bs�Bs�Bs�Br�B�(B�B��B� B�!B�wB��B��B��B��B��B��B��B��B��B�B�uB�aB�4B�B��B��B��B��B�{B�`B��B��B�NB�+B|
Bx�B~Bw�BX9BRBN�BJ�BD�B>�B7qB*&B�B�B�B�B0B��B��B��B�B�MB�B�B��B��B�sB�:B��B��B{Bd�B^dBPB<�B+4B�B
rB
�B
�B
��B
�aB
��B
��B
|B
g�B
^rB
T1B
I�B
?�B
;�B
4tB
*9B
�B
�B
BB	�B	�B	�UB	�B	��B	�kB	��B	��B	yB	g�B	XUB	R.B	I�B	?�B	+JB	�B	�B	�B		}B	jB	RB�>B�4B�#B�B��B��B��B��B�B߃B�bB�8B�B��B��B��B�nB�iB�gB�^B�MB�-B�B��B��B��B��B��B�{B�dB�UBGB~AB~@B�UB�}B��B��B��B��B��B��B��B��B��B�}B�mB�fB�aB�ZB~CBz)By#By!B{1B|5B|6B|7B}<B|9B}7B};B}9B}=B}?B};B|4B{0Bz'BvBxBwBwBuBtBu	BtBtBr�BtB{.B�VB{-BwBxB�aB��B��B��B��B��B��B��B��B�B�)B�+B�'B�#B�1B�8B�9B�?B�JB�OB�MB�cB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�*B�?B�QB�aB��B�B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�"B�&B�&B�3B�GB�CB�BB�9B�@B	XB	]B	hB	nB	B	�B	�B	�B	�B	�B	B	#B	'5B	3�B	7�B	8�B	9�B	=�B	=�B	C�B	D�B	D�B	D�B	H�B	LB	LB	KB	UFB	WPB	XZB	]|B	a�B	a�B	b�B	f�B	j�B	m�B	n�B	p�B	q�B	r�B	uB	z"B	|.B	|,B	}8B	�HB	�PB	�YB	�fB	�sB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�!B	�&B	�/B	�;B	�AB	�OB	�WB	�bB	�hB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�*B	�-B	�1B	�2B	�6B	�7B	�HB	�TB	�XB	�YB	�[B	�aB	�cB	�rB	�}B	߀B	߀B	߁B	��B	�B	�B	�B	�B	�B	��B	߁B	߁B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�.B	�:B	�=B	�<B	�:B	�CB
KB
MB
RB
XB
]B
bB
dB
aB
gB
qB
sB
sB
lB
jB
jB
iB
sB
	}B

�B

�B
	|B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
#�B
, B
5mB
95B
BB
I�B
N�B
T�B
ZKB
`�B
d�B
i
B
l3B
p2B
vmB
{sB
~�B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.004(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940232019060409402320190604094023  AO  ARCAADJP                                                                    20181121041153    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041153  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041153  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094023  IP                  G�O�G�O�G�O�                
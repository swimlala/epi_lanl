CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:37Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141337  20220204114411  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؃���1   @؃8㜖@7Q���c���l�D1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0ffB8  B@  BH  BP  BX  Ba��Bg��Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC�fC�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>y�D>��D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO�fDP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DVy�DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy�D�&fD�\{D���D�� D��D�h�D��
D���D�  D�[3D���D���D�"�D�UDڐ�D��\D��D�S3D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B 33B(33B033B7��B?��BG��BO��BW��BafgBgfgBo��Bw��B��B��fB��fB��fB��fB��3B��fB��fB��3B��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3CٙCٙCٙC�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%�gD&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.�3D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>vgD>�gD?vgD?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ�3DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO�3DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DVvgDV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt�gDy��D�$�D�Z�D��=D��fD�3D�g
D��pD��=D�fD�Y�D��)D��3D�!GD�S�Dڏ
D���D��D�Q�D��D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�n�A�=qA���A��mA���AӰ!AӑhAӋDA�~�A�^5A�;dA�{A��A���A���A���A��A��HA��;A��;A���AҲ-Aџ�A���AϼjA�jAʗ�A��#A�7LA�7LA�ȴA�-A���A���A���A�(�A��A���A�O�A�=qA���A�hsA�5?A��A�A��#A��!A��+A��A�$�A�I�A���A�?}A��7A���A�-A��DA�ZA�  A�ZA��A���A�oA��A�I�A��A��HA��A���A�S�A��;A�x�A���A�hsA��HA�1A��A��A�jA�E�A��jA�A��^A��;A�JA��`A��A�oA��A���A�l�A��9A�7LA��A�hsA��!A�/A�p�A�jA~�+A{l�AyS�Ax��Aw�Avr�At��AsAs�Ap�+Al�/Alr�AkO�Ai�PAhȴAg��Af9XAet�Ad��AchsAa�
A`~�A_��A^Q�A\��A[�FA[/AZQ�AYhsAWx�AV�AU�hAT��AR�\AQ�PAP��AP�AO`BAN9XAM33AL �AJ��AJ��AJZAH�AHA�AHJAG�PAE�AC�7AB�!AAXA@bNA@bA>��A>5?A=�A<��A<E�A;XA:��A:$�A7�#A7��A7�A7oA6E�A5�A4�A3t�A2�9A2M�A2 �A1t�A0��A/�^A/+A.�RA-hsA,v�A,=qA,1'A,{A)l�A(^5A'hsA&��A&�\A&^5A&1A%+A$�uA$1A"=qA!t�A!7LA ��A��A�A�\A{A��A��Al�A��A��A1A33AA�A��AJA�AVA"�A��A�/A��A��A�RA(�Ar�A"�A�yAp�Ar�A"�AM�A��A��At�AC�A�Av�A�A+A ff@�C�@�5?@�p�@��H@��@�\)@�?}@�b@�"�@�@��#@��@���@�33@��@�7L@��m@��y@ꗍ@�V@��@���@���@�-@�K�@�G�@ە�@�7L@�ƨ@֟�@�/@ԛ�@ӝ�@�v�@с@�&�@Ϯ@�@��`@�b@˥�@���@ɺ^@�1'@��;@ǅ@���@��@�hs@�bN@Å@�~�@��@��@��@�@�%@�r�@�1'@��;@���@��y@�@�&�@�|�@�@���@�bN@�(�@���@�J@��u@��w@�\)@�+@�@��@��\@�M�@��7@��9@�b@�;d@�=q@��@��@���@�z�@��y@�-@���@��7@���@��;@��H@���@�v�@���@��@�Ĝ@�Q�@� �@�b@�  @��;@���@��@�S�@��@�V@�J@�^5@�$�@��@��@��^@�x�@�7L@��D@��@��F@���@�|�@�+@�n�@�J@��@��@��#@���@���@��h@�/@�z�@�9X@�b@�ƨ@�;d@�S�@�+@�@���@���@���@���@�ff@���@��@��T@���@�1@��
@���@��w@���@��@��@���@�+@��@��\@�v�@��+@��!@�ȴ@���@�-@�-@�-@�5?@���@���@�~�@�E�@�$�@��@��@��@���@���@���@��m@��P@�dZ@��@�33@�\)@���@�|�@��H@�v�@��@���@��D@��u@���@��9@��@��@��@��@��P@�+@���@���@���@�n�@�$�@�=q@��#@�@��-@�p�@��@��j@��@�Q�@��@�(�@�Z@�bN@�Q�@�A�@�(�@�1@��;@�ƨ@��F@���@�K�@�"�@��H@��!@���@�~�@�v�@�M�@�$�@�J@���@���@��@���@���@�p�@�/@��@��@�V@�%@���@���@���@�Q�@��D@z��@q;@h�p@`PH@X�9@Rn�@L��@E��@@,=@:($@5p�@0"h@*��@%��@!�@�@]�@ݘ@�+@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�n�A�=qA���A��mA���AӰ!AӑhAӋDA�~�A�^5A�;dA�{A��A���A���A���A��A��HA��;A��;A���AҲ-Aџ�A���AϼjA�jAʗ�A��#A�7LA�7LA�ȴA�-A���A���A���A�(�A��A���A�O�A�=qA���A�hsA�5?A��A�A��#A��!A��+A��A�$�A�I�A���A�?}A��7A���A�-A��DA�ZA�  A�ZA��A���A�oA��A�I�A��A��HA��A���A�S�A��;A�x�A���A�hsA��HA�1A��A��A�jA�E�A��jA�A��^A��;A�JA��`A��A�oA��A���A�l�A��9A�7LA��A�hsA��!A�/A�p�A�jA~�+A{l�AyS�Ax��Aw�Avr�At��AsAs�Ap�+Al�/Alr�AkO�Ai�PAhȴAg��Af9XAet�Ad��AchsAa�
A`~�A_��A^Q�A\��A[�FA[/AZQ�AYhsAWx�AV�AU�hAT��AR�\AQ�PAP��AP�AO`BAN9XAM33AL �AJ��AJ��AJZAH�AHA�AHJAG�PAE�AC�7AB�!AAXA@bNA@bA>��A>5?A=�A<��A<E�A;XA:��A:$�A7�#A7��A7�A7oA6E�A5�A4�A3t�A2�9A2M�A2 �A1t�A0��A/�^A/+A.�RA-hsA,v�A,=qA,1'A,{A)l�A(^5A'hsA&��A&�\A&^5A&1A%+A$�uA$1A"=qA!t�A!7LA ��A��A�A�\A{A��A��Al�A��A��A1A33AA�A��AJA�AVA"�A��A�/A��A��A�RA(�Ar�A"�A�yAp�Ar�A"�AM�A��A��At�AC�A�Av�A�A+A ff@�C�@�5?@�p�@��H@��@�\)@�?}@�b@�"�@�@��#@��@���@�33@��@�7L@��m@��y@ꗍ@�V@��@���@���@�-@�K�@�G�@ە�@�7L@�ƨ@֟�@�/@ԛ�@ӝ�@�v�@с@�&�@Ϯ@�@��`@�b@˥�@���@ɺ^@�1'@��;@ǅ@���@��@�hs@�bN@Å@�~�@��@��@��@�@�%@�r�@�1'@��;@���@��y@�@�&�@�|�@�@���@�bN@�(�@���@�J@��u@��w@�\)@�+@�@��@��\@�M�@��7@��9@�b@�;d@�=q@��@��@���@�z�@��y@�-@���@��7@���@��;@��H@���@�v�@���@��@�Ĝ@�Q�@� �@�b@�  @��;@���@��@�S�@��@�V@�J@�^5@�$�@��@��@��^@�x�@�7L@��D@��@��F@���@�|�@�+@�n�@�J@��@��@��#@���@���@��h@�/@�z�@�9X@�b@�ƨ@�;d@�S�@�+@�@���@���@���@���@�ff@���@��@��T@���@�1@��
@���@��w@���@��@��@���@�+@��@��\@�v�@��+@��!@�ȴ@���@�-@�-@�-@�5?@���@���@�~�@�E�@�$�@��@��@��@���@���@���@��m@��P@�dZ@��@�33@�\)@���@�|�@��H@�v�@��@���@��D@��u@���@��9@��@��@��@��@��P@�+@���@���@���@�n�@�$�@�=q@��#@�@��-@�p�@��@��j@��@�Q�@��@�(�@�Z@�bN@�Q�@�A�@�(�@�1@��;@�ƨ@��F@���@�K�@�"�@��H@��!@���@�~�@�v�@�M�@�$�@�J@���@���@��@���@���@�p�@�/@��@��@�V@�%@���@���@���G�O�@��D@z��@q;@h�p@`PH@X�9@Rn�@L��@E��@@,=@:($@5p�@0"h@*��@%��@!�@�@]�@ݘ@�+@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�PB�jB��B��B��B��B��B�B�sB(�B>wBA�B@�BB�BD�BH�BM�BT�B\)B[#B]/Bp�Bu�B�PB��B�B�RBÖBƨBĜBĜB��B��B��B��B��BĜB�qB�3B�9B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�=B�%B�Bk�Be`B^5BT�BA�B)�B�B�B�B{B
=B  B��B��B�B�B�fB�/B��B��BɺBƨB�wB�-B��B�oBv�BT�BJ�B>wB-B�B1B
�B
�ZB
�HB
�B
�qB
�9B
��B
��B
�B
m�B
[#B
W
B
S�B
K�B
B�B
8RB
33B
(�B
VB
+B
B	��B	�B	�B	�NB	�)B	�B	��B	ŢB	�jB	�FB	�'B	��B	��B	��B	��B	�{B	�%B	�B	x�B	r�B	iyB	bNB	\)B	YB	T�B	N�B	G�B	A�B	8RB	5?B	2-B	)�B	#�B	!�B	�B	�B	DB	
=B	+B	B	B	B��B��B��B��B��B�B�B�yB�mB�fB�ZB�BB�/B�B��B��B��B��BɺBɺBƨBÖBB��B�wB�dB�^B�RB�LB�!B�B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�PB�=B�7B�B� Bz�Bx�Bv�Bs�Bo�Bk�BiyBcTB]/B\)B\)B[#B[#BZBXBR�BM�BI�BE�BC�BB�B@�B>wB=qB<jB<jB<jB:^B:^B:^B:^B9XB7LB6FB6FB49B33B2-B49B6FB33B6FB49B49B49B5?B7LB8RB7LB7LB6FB5?B7LB49B<jB>wB:^B6FB49B;dB>wB@�B=qB>wBE�BG�BF�BL�BQ�BQ�BR�BR�BT�BVBXBXBYBZB\)B\)B]/B]/B^5B_;BcTBe`BgmBgmBhsBiyBiyBiyBjBl�Bm�Bq�Bt�Bw�Bw�Bw�Bx�B|�B�B�B�B�B�%B�%B�+B�+B�=B�PB�\B�hB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�FB�XB�}BBĜBƨBȴB��B��B��B��B��B�B�`B�sB�yB�B�B�B�B��B��B	  B	B	B	%B	1B	JB	PB	VB	bB	hB	uB	�B	�B	�B	"�B	$�B	'�B	-B	.B	2-B	;dB	>wB	A�B	D�B	F�B	G�B	I�B	M�B	O�B	Q�B	O�B	N�B	N�B	N�B	O�B	S�B	XB	^5B	^5B	_;B	_;B	`BB	cTB	e`B	hsB	jB	l�B	l�B	m�B	p�B	u�B	y�B	z�B	|�B	~�B	� B	�B	�B	�B	�B	�%B	�B	�%B	�%B	�%B	�+B	�DB	�VB	�oB	�oB	�oB	�oB	�bB	�\B	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�-B	�?B	�?B	�LB	�RB	�^B	�jB	�qB	��B	ÖB	ĜB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�;B	�;B	�;B	�BB	�BB	�BB	�NB	�NB	�TB	�)B	�PB
	7B
�B
�B
#TB
,�B
2�B
9�B
?�B
ESB
KB
O\B
TB
Y�B
_�B
d�B
h�B
k�B
qAB
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B{�B�(B�?BƳBǹBǹB��B��B��B�FB�B5DB8VB7PB9\B;hB?�BD�BK�BR�BQ�BS�BgnBl�B�B��B��B�B�\B�nB�bB�bBBȲBȲBɸBŠB�dB�:B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�ZB�$B�B|�Bw�BbVB\2BUBK�B8^B �B�BeBeBTBB��B�B��B�B�gB�CB�B��B¦B��B��B�WB�B��B�RBm�BK�BA�B5aB#�B�B
� B
�B
�LB
�:B
�	B
�fB
�.B
��B
��B
yB
d�B
R B
NB
J�B
B�B
9�B
/RB
*3B
�B
YB	�/B	�B	��B	�B	�B	�UB	�0B	�B	��B	��B	�tB	�QB	�2B	��B	��B	��B	��B	��B	}4B	xB	o�B	i�B	`�B	Y`B	S;B	P*B	LB	E�B	>�B	8�B	/gB	,UB	)CB	!B	�B	�B	�B	�B	]B	VB�EB�3B�,B� B�B�	B��B��B��B��B��B��BފB݃B�wB�`B�MB�;B�B��B��B��B��B��B��B��B��B��B��B��B�B�tB�nB�DB�+B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�vB�cB�]B{@Bw'Br	Bo�Bm�Bj�Bf�Bb�B`�BZBTZBSTBSTBRNBRNBQHBO<BJBE B@�B<�B:�B9�B7�B5�B4�B3�B3�B3�B1�B1�B1�B1�B0�B.|B-vB-vB+iB*cB)^B+jB-vB*dB-wB+jB+jB+jB,pB.}B/�B.}B.}B-wB,pB.}B+jB3�B5�B1�B-xB+kB2�B5�B7�B4�B5�B<�B>�B=�BC�BIBIBJ#BJ#BL/BM5BOABOABPHBQNBSZBSZBT`BT`BUfBVlBZ�B\�B^�B^�B_�B`�B`�B`�Ba�Bc�Bd�Bh�Bk�Bn�Bn�Bn�BpBtBy<B{IB|OB|OB}UB}UB~[B~[B�lB�B��B��B��B��B��B��B��B��B��B��B��B�B�$B�<B�BB�BB�[B�sB��B��B��B��B��B��B��B��B��B�B�$B�HB܊BߝB�B�B��B��B��B��B�
B�)B�/B�5B�MB�YB	rB	xB	~B	�B	�B	
�B	�B	�B	�B	�B	B	B	$4B	%:B	)SB	2�B	5�B	8�B	;�B	=�B	>�B	@�B	D�B	GB	IB	GB	E�B	E�B	E�B	GB	KB	O3B	UXB	UXB	V^B	V^B	WeB	ZwB	\�B	_�B	a�B	c�B	c�B	d�B	g�B	l�B	p�B	rB	tB	vB	w!B	y-B	z3B	{:B	|@B	}EB	|@B	}FB	}FB	}FB	~LB	�dB	�vB	��B	��B	��B	��B	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�,B	�EB	�KB	�KB	�]B	�]B	�jB	�pB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�,B	�,B	�9B	�9B	�?B	�?B	�DB	�JB	�VB	�VB	�VB	�]B	�]B	�]B	�iB	�iG�O�B	�DB	�jB
 PB

�B
�B
lB
$B
)�B
1B
6�B
<jB
B&B
FsB
K)B
P�B
V�B
\B
_�B
b�B
hWB
i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144112022020411441120220204114411  AO  ARCAADJP                                                                    20200618141337    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141337  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141337  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114411  IP                  G�O�G�O�G�O�                
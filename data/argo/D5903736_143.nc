CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-26T02:15:23Z AOML 3.0 creation; 2016-05-31T19:14:48Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7$   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7(   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7,   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7<   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7L   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7\   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7d   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8    DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8@   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8h   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
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
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20160326021523  20190604094000  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_143                   2C  D   APEX                            5368                            041511                          846 @ן��N:�1   @ן�3��K@3�������dBz�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @@  @�  @�  A   A   A@  A^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�3D�c3D���D��3D�fD�@ D�� D��3D��3D�L�D��3D�ɚD���D�6fDړ3D�� D� D�9�D�vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @<��@|��@�ff@�ffA33A?33A]��A}��A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC �C�CٙC�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C2�C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CN�CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D=3D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dtp Dy�gD��D�a�D�� D���D��D�>fD��fD���D��D�K3D���D�� D��3D�4�Dڑ�D�fD�fD�8 D�t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yA��/A���A˺^A˩�Aˇ+A�S�A��A��A�ȴAʑhAʉ7Aʉ7AʋDAʏ\Aʇ+AʅAʁAʁA�z�A�n�A�hsA�^5A�`BA�\)A�XA�VA�O�A�K�A�E�A�7LA�
=A��HA��#A���A���AɼjAɕ�A�9XA���AȰ!A�hsA�M�A�I�A�=qA�"�A�A���A���A�A�{A�O�A�XA�1'A�VA��HA�Aǰ!AǗ�A�|�A�;dA��A�?}Að!A�A��A���A��+A��TA�?}A�\)A��A�E�A���A�|�A���A�$�A���A��A��PA���A���A�z�A��/A�v�A���A�A���A�K�A��A���A�?}A��!A�A�A��A�VA�C�A�O�A��;A� �A�n�A��A�%A���A�
=A���A�7LA���A��\A��A��A��-A�?}A��PA��+A~�\A{&�Aw��As�Ap$�An�RAlȴAj��Ag�Ag`BAf��Af�jAfffAdĜAc�PAb��A_�-A]|�A\��A\Q�A[dZAUdZAQ��AP-AN�RAM�TAMVAJ9XAHĜAF~�AD��AB�`AA��A@��A>�uA=�FA;�A8��A7�A6r�A5�A5O�A5VA4�DA3��A2��A0  A.~�A-\)A+�A*r�A)VA'�hA&�jA%��A$��A#��A#"�A"~�A"5?A!�A �`AS�A�HAt�An�At�A{A33A�FAM�A�hAr�A�An�A9XA�-A(�A�`A�AoA
=A��A�7A
�!A
9XA	G�A-AhsAQ�Av�A�#A��Ap�AG�A�A�`A��A ��@��@�~�@���@���@�K�@���@�K�@��H@�p�@�w@�v�@�hs@�bN@�@�l�@�@�b@�|�@��@�+@��@�dZ@�E�@���@��@ݺ^@�V@�;d@�=q@�hs@��@��/@ו�@�X@�l�@��T@�33@��@���@͡�@�hs@˶F@�J@��/@��
@ư!@���@Ý�@�@��H@���@��@���@��@�z�@�(�@�t�@��@��!@���@��`@�I�@��;@�
=@�^5@�@���@��`@�bN@�Q�@�o@��^@�G�@��j@�A�@��;@��@���@���@��@���@��@��@��#@�G�@�z�@�t�@���@�v�@�E�@�=q@��#@��/@��9@���@���@��@��D@��j@��@�Z@��@�l�@��+@�/@��`@��P@�bN@��@��m@��+@���@�`B@�?}@���@���@�I�@�1'@�  @� �@��@�ff@�/@���@��9@���@��D@�1'@�  @��@�ƨ@�dZ@�
=@�\)@�+@�-@�?}@�Ĝ@���@�bN@��D@�Ĝ@�V@�hs@���@�J@��T@���@��j@��F@���@�ȴ@���@�$�@�{@�5?@���@��\@�@��!@���@�V@��^@�`B@�&�@���@��@�j@�j@�j@�|�@�K�@�;d@�
=@��+@��\@�n�@�-@��@�=q@��R@�^5@�-@��@��^@���@�X@�7L@�V@���@��@��@��j@���@��/@��9@��`@��/@��@��@��m@��@��@�|�@�\)@�\)@�;d@�C�@�@���@�J@�p�@��u@�A�@��;@�\)@�;d@�33@�+@��@�
=@��+@��@��T@���@��^@���@�X@���@��u@� �@� �@��@��@��F@���@���@�S�@�-@��@���@�@�@�7L@���@��/@�Ĝ@��@�r�@�1'@�9X@�r�@�A�@��m@�t�@�K�@�;d@�
=@��!@��+@���@��@�"�@�S�@�+@���@��y@���@���@��R@��@��/@���@��j@�9X@���@}/@t�j@m?}@g�@]�@U?}@M�-@F@Ax�@:�@4��@.{@(��@"��@{@�#@�F@�u@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A��yA��/A���A˺^A˩�Aˇ+A�S�A��A��A�ȴAʑhAʉ7Aʉ7AʋDAʏ\Aʇ+AʅAʁAʁA�z�A�n�A�hsA�^5A�`BA�\)A�XA�VA�O�A�K�A�E�A�7LA�
=A��HA��#A���A���AɼjAɕ�A�9XA���AȰ!A�hsA�M�A�I�A�=qA�"�A�A���A���A�A�{A�O�A�XA�1'A�VA��HA�Aǰ!AǗ�A�|�A�;dA��A�?}Að!A�A��A���A��+A��TA�?}A�\)A��A�E�A���A�|�A���A�$�A���A��A��PA���A���A�z�A��/A�v�A���A�A���A�K�A��A���A�?}A��!A�A�A��A�VA�C�A�O�A��;A� �A�n�A��A�%A���A�
=A���A�7LA���A��\A��A��A��-A�?}A��PA��+A~�\A{&�Aw��As�Ap$�An�RAlȴAj��Ag�Ag`BAf��Af�jAfffAdĜAc�PAb��A_�-A]|�A\��A\Q�A[dZAUdZAQ��AP-AN�RAM�TAMVAJ9XAHĜAF~�AD��AB�`AA��A@��A>�uA=�FA;�A8��A7�A6r�A5�A5O�A5VA4�DA3��A2��A0  A.~�A-\)A+�A*r�A)VA'�hA&�jA%��A$��A#��A#"�A"~�A"5?A!�A �`AS�A�HAt�An�At�A{A33A�FAM�A�hAr�A�An�A9XA�-A(�A�`A�AoA
=A��A�7A
�!A
9XA	G�A-AhsAQ�Av�A�#A��Ap�AG�A�A�`A��A ��@��@�~�@���@���@�K�@���@�K�@��H@�p�@�w@�v�@�hs@�bN@�@�l�@�@�b@�|�@��@�+@��@�dZ@�E�@���@��@ݺ^@�V@�;d@�=q@�hs@��@��/@ו�@�X@�l�@��T@�33@��@���@͡�@�hs@˶F@�J@��/@��
@ư!@���@Ý�@�@��H@���@��@���@��@�z�@�(�@�t�@��@��!@���@��`@�I�@��;@�
=@�^5@�@���@��`@�bN@�Q�@�o@��^@�G�@��j@�A�@��;@��@���@���@��@���@��@��@��#@�G�@�z�@�t�@���@�v�@�E�@�=q@��#@��/@��9@���@���@��@��D@��j@��@�Z@��@�l�@��+@�/@��`@��P@�bN@��@��m@��+@���@�`B@�?}@���@���@�I�@�1'@�  @� �@��@�ff@�/@���@��9@���@��D@�1'@�  @��@�ƨ@�dZ@�
=@�\)@�+@�-@�?}@�Ĝ@���@�bN@��D@�Ĝ@�V@�hs@���@�J@��T@���@��j@��F@���@�ȴ@���@�$�@�{@�5?@���@��\@�@��!@���@�V@��^@�`B@�&�@���@��@�j@�j@�j@�|�@�K�@�;d@�
=@��+@��\@�n�@�-@��@�=q@��R@�^5@�-@��@��^@���@�X@�7L@�V@���@��@��@��j@���@��/@��9@��`@��/@��@��@��m@��@��@�|�@�\)@�\)@�;d@�C�@�@���@�J@�p�@��u@�A�@��;@�\)@�;d@�33@�+@��@�
=@��+@��@��T@���@��^@���@�X@���@��u@� �@� �@��@��@��F@���@���@�S�@�-@��@���@�@�@�7L@���@��/@�Ĝ@��@�r�@�1'@�9X@�r�@�A�@��m@�t�@�K�@�;d@�
=@��!@��+@���@��@�"�@�S�@�+@���@��y@���@���@��R@��@��/@���@��jG�O�@���@}/@t�j@m?}@g�@]�@U?}@M�-@F@Ax�@:�@4��@.{@(��@"��@{@�#@�F@�u@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
o�B
o�B
o�B
o�B
o�B
p�B
q�B
s�B
u�B
w�B
z�B
z�B
|�B
|�B
~�B
� B
�B
�B
�B
�B
�B
�B
�+B
�7B
�DB
�JB
�PB
�VB
�VB
�VB
�\B
�hB
��B
��B
��B
��B
�B
ĜB
�fB
�B
��B+BJBoB�B�B�B�B�B)�BC�Bp�B�bB��B��B��B��B�B�^B�#B��B"�BO�B��B�TB�B��B�B�B�sB�)BǮBÖBÖBBƨBŢBB�RB��B�uB�PB�1B�B}�Bu�Br�BjBXBP�BD�B�B�#B��B�RB��B��B�hB�oB� BjBXBH�B8RB%�BB
�yB
B
�B
�uB
w�B
aHB
YB
N�B
>wB
-B
hB	�B	�
B	�dB	�B	��B	��B	��B	��B	��B	��B	��B	�VB	�+B	�B	s�B	hsB	bNB	^5B	S�B	8RB	&�B	�B	�B	{B	VB	B��B��B�B�B�`B�NB�/B�B��B��B��B��B��B��B��B��B��B��BǮBɺBȴBȴB��B��B��B��BǮBŢB�}B�dB�dB�qB�}B�wB�XB�9B�B��B��B��B��B�bB�DB�7B�7B�1B�1B�+B�B�B�B~�B~�B}�B|�B{�B|�B{�Bv�Bt�Bs�Bs�Bu�Bu�Bv�Bv�Bw�Bx�Bw�Bx�Bw�By�By�Bx�Bw�Bu�Bw�Bx�B|�B}�B}�B}�B|�Bz�Bw�Bu�Bv�Bw�Bw�Bw�Bv�Bv�Bt�Bx�By�By�By�Bx�By�B{�B}�B}�B}�B}�B�B�B�%B�DB�oB�{B��B��B��B��B��B��B��B��B��B�B�B�B�!B�FB�LB�RB�dB�qB�wB��BŢBȴBɺB��B��B��B�B�B�B�#B�#B�/B�BB�BB�NB�ZB�mB�sB�B�B��B	  B	B	B	B	B	B	%B	%B	1B		7B		7B	
=B	hB	uB	�B	�B	�B	�B	 �B	#�B	+B	-B	0!B	1'B	1'B	2-B	5?B	?}B	B�B	B�B	A�B	D�B	F�B	H�B	I�B	I�B	L�B	L�B	O�B	P�B	XB	VB	S�B	YB	]/B	_;B	aHB	dZB	gmB	iyB	m�B	o�B	p�B	v�B	x�B	w�B	y�B	z�B	z�B	|�B	�B	�%B	�7B	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�'B	�'B	�!B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�FB	�RB	�dB	�dB	�dB	�dB	�wB	B	B	B	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�#B	�)B	�#B	�#B	�B	�)B	�5B	�5B	�/B	�5B	�;B	�BB	�BB	�;B	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�`B	�`B	�fB	�sB	�yB	�yB	�yB	�yB	�sB	�mB	�fB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B	��B	��B	��B
B
  B
DB
�B
!�B
&�B
,B
2-B
:^B
?}B
E�B
K�B
Q�B
XB
^5B
cTB
hsB
m�B
q�B
v�B
y�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B
prB
pvB
p~B
pyB
pyB
q}B
r�B
t�B
v�B
x�B
{�B
{�B
}�B
}�B
�B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�%B
�.B
�0B
�4B
�0B
�7B
�CB
�nB
�{B
��B
��B
��B
�tB
�CB
�B
��B	B&BIBfB�ByByB�B*�BDsBq�B�BB��B��B��B��B��B�:B�B��B#�BP�B��B�5B�sB��B�B�qB�VB�
BȑB�pB�uB�lBǅBƁB�lB�/B��B�UB�2B�B��B~�Bv�Bs�BkYBX�BQ�BEwB��B��B��B�-B��B�rB�CB�HB��BkWBX�BI�B9.B&�B�B
�VB
�iB
��B
�NB
x�B
b'B
Y�B
O�B
?SB
-�B
AB	�B	��B	�@B	��B	��B	��B	��B	��B	��B	�wB	�fB	�-B	�B	��B	t�B	iFB	c%B	_B	T�B	9)B	'�B	�B	eB	SB	.B	�B��B��B�wB�YB�5B�'B�B��B��BͨBΩB��B��B��B��BѽBиB̟BȄBʏBɋBɎB̤BйBϰB˗BȇB�{B�RB�<B�9B�HB�TB�NB�-B�B��B��B��B�}B�]B�9B�B�B�B�B�
B�B��B��B��B�B�B~�B}�B|�B}�B|�Bw�Bu�Bt�Bt�Bv�Bv�Bw�Bw�Bx�By�Bx�By�Bx�Bz�Bz�By�Bx�Bv�Bx�By�B}�B~�B~�B~�B}�B{�Bx�Bv�Bw�Bx�Bx�Bx�Bw�Bw�Bu�By�Bz�Bz�Bz�By�Bz�B|�B~�B~�B~�B~�B��B��B��B�B�DB�PB�XB�VB�lB�uB�sB�zB��B��B��B��B��B��B��B�B�%B�(B�:B�HB�OB�]B�|BɋBʏB͡B��B��B��B��B��B��B��B�B�B�B�&B�4B�CB�MB�eB�sB��B	 �B	�B	�B	�B	�B	�B	�B	�B		B	
B	
B	B	>B	OB	[B	kB	wB	�B	!�B	$�B	+�B	-�B	0�B	1�B	2 B	3B	6B	@VB	CdB	CgB	B`B	EvB	G�B	I�B	J�B	J�B	M�B	M�B	P�B	Q�B	X�B	V�B	T�B	Y�B	^B	`B	bB	e/B	hHB	jQB	niB	ptB	q{B	w�B	y�B	x�B	z�B	{�B	{�B	}�B	��B	��B	�B	�*B	�BB	�YB	�yB	��B	�rB	�eB	�`B	�kB	�yB	�xB	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�;B	�@B	�=B	�>B	�PB	�kB	�iB	�jB	�uB	�{B	�{B	�|B	�yB	�zB	�|B	�zB	�{B	ǂB	ʔB	̟B	ͪB	϶B	зB	ϳB	иB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�(B	�+B	�7B	�1B	�7B	�8B	�@B	�9B	�7B	�AB	�LB	�TB	�OB	�TB	�RB	�NB	�HB	�@B	�@B	�EB	�EB	�SB	�ZB	�bB	�^B	�\B	�iB	�uB	�}B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B	��B	��B	��B
�G�O�B
B
tB
"�B
'�B
,�B
3B
;:B
@WB
FzB
L�B
R�B
X�B
_B
d.B
iJB
nkB
r�B
w�B
z�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =0.001(+/-0.002) in PSS-78.                                                                                                                                                                                             Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940002019060409400020190604094000  AO  ARCAADJP                                                                    20160326021523    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160326021523  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160326021523  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094000  IP                  G�O�G�O�G�O�                
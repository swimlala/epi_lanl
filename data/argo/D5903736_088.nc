CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-09-10T00:02:49Z AOML 3.0 creation; 2016-05-31T19:14:39Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140910000249  20160531121439  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               XA   AO  4051_7090_088                   2C  D   APEX                            5368                            041511                          846 @������1   @���'�@3��;dZ�d��+J1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    XA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB6ffB?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D��D�@ D�s3D�� D��fD�9�D��fD�� D�fD�<�D��fD��fD�	�D�)�Dڜ�D��D�fD�FfD�|�D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B033B633B?fgBG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D3D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D3D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��DvgD��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��DtvgDy��D�3D�>fD�q�D��fD���D�8 D���D��fD��D�;3D���D���D� D�( Dڛ3D�3D��D�D�D�{3D�a�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�^5A�bNA�ffA�hA��A�-A�ȴA�ȴA�ȴA�ƨA�ĜA���A�jA�9A�!A�A�A�A��A�DA�wA�^5A�t�Aԥ�A���A��A�1A�A��yA�bNA�1'AȃA�x�Aư!A�7LA���A�A�A�hsA���A���A�A�A��;A�ZA��A�I�A�(�A�hsA�t�A�/A��7A��A��uA��`A�;dA���A���A�~�A��7A�C�A��`A�ZA���A�`BA�|�A�ĜA�+A���A���A��
A�?}A��A��mA�+A�5?A�ĜA��A�ȴA�=qA���A�ffA�ƨA��/A��`A�S�A��^A�;dA�ZA�n�A��A�-A��DA�O�A���A��A��9A��^A��hA�|�A���A� �A�|�A�
=A��/A���A�5?A�A��A33A}&�A{�Az�jAz{Ay\)Ax��Ax{Av1'Ar�/ArE�Ar�Ap��An1Ak��Aj��AiAf�Ac��A`�DA^ZA]A\�A["�AZ9XAXbNAV��AU��AU%ARz�AP�jAP�AO�-ANI�AL��AKC�AIXAH{AGO�AE�
AD-AC�AB�jAA�A@1'A?�A=�hA<��A;��A:^5A9oA7�A5�PA2Q�A/�A-�A-/A,��A+��A)�A&^5A$��A#"�A!��A JA/A�HAQ�A�wAA(�A%A�jA\)A�uAdZAA�AVA  A{A�-Al�A�A�Av�A�
A�A	A{A33A�uA��A�RA��A�\A�
A�`AjA�A ��A �jA ��A b@�M�@�@��@�hs@��@��@�5?@��`@���@�@��h@�\)@��;@�
=@���@���@��9@�D@�I�@��m@�@�|�@�@��;@��@�+@�J@�&�@��@�  @�ȴ@�\@�~�@�@�@��y@�33@���@�R@��@�hs@��@�x�@�X@�?}@��@���@�@�C�@�7@�bN@�(�@�o@�z�@�o@ާ�@��@��@�E�@�7L@�ff@��
@�|�@�@�C�@�/@�1'@�+@��@�|�@ģ�@�`B@�|�@���@�"�@�$�@���@��@��h@�~�@�X@�C�@��T@�O�@�G�@�Q�@�9X@�1'@��H@�E�@�$�@�@�?}@��j@���@��D@�z�@�Q�@��@�C�@��-@�j@���@�ƨ@���@��P@�dZ@�\)@�S�@�"�@���@�=q@�{@���@��@�%@���@��j@�z�@��
@���@��P@�t�@�\)@�S�@�
=@�ȴ@�~�@�^5@�M�@�^5@��@�E�@�@��@���@��D@�Z@��w@�\)@���@���@���@���@���@���@�5?@�`B@��@���@��D@��j@��D@�9X@�Q�@��m@���@�@��\@�V@��@���@�x�@�`B@�V@��j@��u@���@��u@��D@�r�@�A�@�(�@� �@� �@� �@� �@�(�@��@�  @�b@�Z@�Z@�Z@�Z@�Q�@�I�@�A�@�(�@�(�@��@���@��@��
@��m@��F@��y@�@�X@�/@�&�@��9@�9X@���@��F@��m@��@�b@�  @�l�@�K�@�o@��y@�ȴ@�$�@���@�p�@�/@��@�Ĝ@���@��u@�r�@�Z@��;@���@�S�@�
=@���@���@�ff@�{@�@��-@��-@���@�x�@�X@�G�@�7L@��`@�1'@��m@���@�|�@�l�@�l�@�K�@�;d@�+@���@�^5@�M�@�J@�X@���@��j@�z�@�I�@�1'@�  @�|�@��!@�ff@�E�@�$�@��@��#@�`B@���@�9X@��m@��P@�\)@��@���@��/@��@~�R@tz�@g��@`�u@Y�@R-@KS�@B��@=V@8b@2�\@*M�@$�@;d@ƨ@�;@I�@A�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�\)A�^5A�bNA�ffA�hA��A�-A�ȴA�ȴA�ȴA�ƨA�ĜA���A�jA�9A�!A�A�A�A��A�DA�wA�^5A�t�Aԥ�A���A��A�1A�A��yA�bNA�1'AȃA�x�Aư!A�7LA���A�A�A�hsA���A���A�A�A��;A�ZA��A�I�A�(�A�hsA�t�A�/A��7A��A��uA��`A�;dA���A���A�~�A��7A�C�A��`A�ZA���A�`BA�|�A�ĜA�+A���A���A��
A�?}A��A��mA�+A�5?A�ĜA��A�ȴA�=qA���A�ffA�ƨA��/A��`A�S�A��^A�;dA�ZA�n�A��A�-A��DA�O�A���A��A��9A��^A��hA�|�A���A� �A�|�A�
=A��/A���A�5?A�A��A33A}&�A{�Az�jAz{Ay\)Ax��Ax{Av1'Ar�/ArE�Ar�Ap��An1Ak��Aj��AiAf�Ac��A`�DA^ZA]A\�A["�AZ9XAXbNAV��AU��AU%ARz�AP�jAP�AO�-ANI�AL��AKC�AIXAH{AGO�AE�
AD-AC�AB�jAA�A@1'A?�A=�hA<��A;��A:^5A9oA7�A5�PA2Q�A/�A-�A-/A,��A+��A)�A&^5A$��A#"�A!��A JA/A�HAQ�A�wAA(�A%A�jA\)A�uAdZAA�AVA  A{A�-Al�A�A�Av�A�
A�A	A{A33A�uA��A�RA��A�\A�
A�`AjA�A ��A �jA ��A b@�M�@�@��@�hs@��@��@�5?@��`@���@�@��h@�\)@��;@�
=@���@���@��9@�D@�I�@��m@�@�|�@�@��;@��@�+@�J@�&�@��@�  @�ȴ@�\@�~�@�@�@��y@�33@���@�R@��@�hs@��@�x�@�X@�?}@��@���@�@�C�@�7@�bN@�(�@�o@�z�@�o@ާ�@��@��@�E�@�7L@�ff@��
@�|�@�@�C�@�/@�1'@�+@��@�|�@ģ�@�`B@�|�@���@�"�@�$�@���@��@��h@�~�@�X@�C�@��T@�O�@�G�@�Q�@�9X@�1'@��H@�E�@�$�@�@�?}@��j@���@��D@�z�@�Q�@��@�C�@��-@�j@���@�ƨ@���@��P@�dZ@�\)@�S�@�"�@���@�=q@�{@���@��@�%@���@��j@�z�@��
@���@��P@�t�@�\)@�S�@�
=@�ȴ@�~�@�^5@�M�@�^5@��@�E�@�@��@���@��D@�Z@��w@�\)@���@���@���@���@���@���@�5?@�`B@��@���@��D@��j@��D@�9X@�Q�@��m@���@�@��\@�V@��@���@�x�@�`B@�V@��j@��u@���@��u@��D@�r�@�A�@�(�@� �@� �@� �@� �@�(�@��@�  @�b@�Z@�Z@�Z@�Z@�Q�@�I�@�A�@�(�@�(�@��@���@��@��
@��m@��F@��y@�@�X@�/@�&�@��9@�9X@���@��F@��m@��@�b@�  @�l�@�K�@�o@��y@�ȴ@�$�@���@�p�@�/@��@�Ĝ@���@��u@�r�@�Z@��;@���@�S�@�
=@���@���@�ff@�{@�@��-@��-@���@�x�@�X@�G�@�7L@��`@�1'@��m@���@�|�@�l�@�l�@�K�@�;d@�+@���@�^5@�M�@�J@�X@���@��j@�z�@�I�@�1'@�  @�|�@��!@�ff@�E�@�$�@��@��#@�`B@���@�9X@��m@��P@�\)@��@���@��/@��@~�R@tz�@g��@`�u@Y�@R-@KS�@B��@=V@8b@2�\@*M�@$�@;d@ƨ@�;@I�@A�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBG�BE�BG�BJ�BVB^5B`BBiyBjBjBjBjBjBk�Bk�Bk�Bk�Bk�Bk�Bk�Bm�B��B�B�B+BC�BP�BVB_;Bl�BjBt�B��B�dB��B��B�B�B�B�NB�TB�TB�TB�NB�ZB�ZB�NB�;B�5B�)B��B�LB��B�uB�+B{�BffBE�B49B.B'�B"�B{B��B��B�FB�B��B��B�%Bm�Be`BYBM�BH�BD�B:^B(�B�B�BVB�B�`B�
B��B��B�RB��B�7Bu�B^5BN�B(�BJB
��B
�mB
�B
��B
�B
��B
�bB
|�B
u�B
q�B
k�B
gmB
bNB
_;B
[#B
P�B
E�B
B�B
?}B
;dB
7LB
2-B
%�B
{B
\B
PB
B	��B	�sB	�HB	��B	ƨB	�FB	��B	��B	�uB	�VB	�=B	�B	y�B	q�B	k�B	ffB	]/B	VB	R�B	O�B	I�B	A�B	<jB	5?B	/B	+B	%�B	�B	�B	�B	{B	VB	
=B	B��B��B��B�B�sB�HB�B��B��BɺBǮBÖB�qB�XB�?B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�B��B��B��B��B��B�B�FB�'B�B��B��B�B�!B�'B�9B�^B�jB�qB�jB�RB�RB�FB�FB�^B�}B��B�/B�BB�fB�B�B�B�B�B��B��B��B��B��B��B	B	B	%B	B	B	B	B	B��B	  B	+B	VB	{B	{B	�B	�B	$�B	&�B	'�B	+B	/B	/B	5?B	8RB	9XB	8RB	5?B	0!B	/B	2-B	8RB	7LB	,B	�B	PB	B�B�B��B�B�BB�)B�5B�HB�yB�B�mB�B�BB�NB�fB�mB�B�B�B�B�B�B�B��B��B��B��B��B��B		7B	VB	\B	bB	hB	hB	bB	hB	hB	uB	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	&�B	'�B	)�B	,B	.B	0!B	1'B	1'B	2-B	9XB	<jB	>wB	>wB	?}B	@�B	C�B	F�B	H�B	I�B	M�B	Q�B	S�B	XB	YB	YB	YB	ZB	ZB	ZB	ZB	\)B	^5B	aHB	cTB	ffB	iyB	l�B	o�B	p�B	p�B	v�B	y�B	{�B	|�B	~�B	�B	�B	�B	�%B	�=B	�JB	�PB	�PB	�PB	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�jB	�}B	��B	��B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�B	�B	�B	�B	�/B	�;B	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�HB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
�B
$�B
,B
2-B
8RB
=qB
C�B
I�B
M�B
R�B
XB
]/B
cTB
iyB
k�B
m�B
r�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BG�BE�BG�BJ�BVB^8B`HBi~Bj�Bj�Bj�Bj�Bj�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bm�B��B�B�B+
BC�BP�BVB_CBl�Bj�Bt�B��B�oB��B��B�B�B�+B�\B�^B�dB�bB�ZB�fB�gB�[B�HB�AB�4B��B�WB��B�{B�8B{�BfjBE�B4AB.B'�B"�B�B��B�B�KB�B��B��B�)Bm�BefBYBM�BH�BD�B:`B(�B�B�BWB�B�dB�B��B��B�VB��B�=Bu�B^8BN�B(�BSB
��B
�vB
�B
��B
�
B
��B
�kB
|�B
u�B
q�B
k�B
g{B
b\B
_FB
[/B
P�B
E�B
B�B
?�B
;pB
7XB
28B
%�B
�B
iB
\B
(B	��B	�B	�UB	�B	ƹB	�UB	��B	��B	��B	�fB	�NB	�+B	y�B	q�B	k�B	fxB	]?B	VB	SB	O�B	I�B	A�B	<�B	5VB	/.B	+B	%�B	�B	�B	�B	�B	lB	
SB	)B�	B��B��B�B�B�`B�'B��B��B��B��BîB��B�rB�YB�GB�<B�.B�#B�B�B�
B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�'B�:B�)B�B��B��B�B�B�5B�`B�AB�B��B��B�B�;B�@B�UB�vB��B��B��B�iB�jB�`B�aB�wB��B�B�GB�YB�|B�B�B��B��B��B��B��B��B��B�B�B	#B	5B	<B	5B	4B	.B	.B	&B�B	 B	?B	iB	�B	�B	�B	�B	$�B	&�B	(B	+B	//B	/.B	5SB	8dB	9jB	8eB	5QB	03B	//B	2BB	8eB	7aB	,B	�B	fB	B�B�B��B�B�YB�?B�MB�]B�B�B�B�B�YB�dB�}B�B�B�B��B�B��B�B��B��B��B��B��B��B�B		LB	lB	qB	wB	B	}B	uB	{B	~B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	&�B	(B	*B	,B	.)B	04B	17B	17B	2@B	9jB	<{B	>�B	>�B	?�B	@�B	C�B	F�B	H�B	I�B	M�B	Q�B	T
B	X"B	Y)B	Y*B	Y)B	Z1B	Z/B	Z0B	Z1B	\:B	^GB	a\B	cdB	fwB	i�B	l�B	o�B	p�B	p�B	v�B	y�B	{�B	|�B	B	�B	�#B	�$B	�6B	�NB	�YB	�bB	�`B	�aB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�<B	�wB	��B	��B	��B	B	åB	ĪB	ůB	ůB	ƷB	��B	��B	��B	�B	�B	�B	�B	�)B	�0B	�0B	�+B	�+B	�%B	�*B	�<B	�JB	�PB	�QB	�PB	�WB	�\B	�]B	�\B	�VB	�VB	�WB	�\B	�bB	�kB	�pB	�mB	�wB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	� B	�B	�B	�B	�B	�B	�B	��B	�B	� B	� B	��B	��B	� B
%B
�B
�B
$�B
,B
27B
8^B
=}B
C�B
I�B
M�B
R�B
XB
]7B
c_B
i�B
k�B
m�B
r�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214392016053112143920160531121439  AO  ARCAADJP                                                                    20140910000249    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140910000249  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140910000249  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121439  IP                  G�O�G�O�G�O�                
CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:27Z AOML 3.0 creation; 2016-05-31T19:14:31Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230527  20160531121431  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               (A   AO  4051_7090_040                   2C  D   APEX                            5368                            041511                          846 @֗���@1   @֗�V�@5bM���d���+1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    (A   B   B   @�ff@�33A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�fD��D�L�D�y�D�� D��fD�L�D��fD��fD� D�I�D�i�D��fD�fD�L�Dړ3D�� D��fD�<�D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���@�ffA33A=��A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt�Dy�3D�3D�K3D�x D��fD���D�K3D���D���D�fD�H D�h D���D��D�K3Dڑ�D��fD���D�;3D�a�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ffA�hsA�bNA�bNA�jA�v�A�z�A�|�A�x�A�|�A�|�AƃAƃA�|�A�p�A�\)A�C�A��Ař�A�33A���A���A�z�A��#AÃA��A���A��!A��\A�dZA�33A��TA���A�9XA��A���A�K�A��A���A��jA��DA�C�A���A�?}A�bA���A��FA�M�A��jA�E�A��;A��jA�S�A��
A�I�A�"�A�p�A���A��hA�bNA�&�A���A��A�r�A���A�l�A�G�A��A�ZA���A�hsA�oA�33A���A�z�A���A�?}A�ĜA���A�M�A���A�33A�A���A��9A���A�A�A��A��PA�~�A�I�A��A�dZA�p�A���A��^A��A��A�bNA��A���A���A��#A���A���A�Q�A��DA�/A�$�A�A�A���A���A���A� �A��+A��-A��A��A��wA�A�dZA��A��\A���A�ffA�ZA�G�A��;A���A~��A|I�A|Ax�/Av��At1'Ar-Ap�Ao�An��Am�wAkp�Ah�Af�\Ae�-Ad�AcƨAb�Ab��AbJA`�9A]x�AY�PAX�`AU�;AS�PAO�;AM�AK\)AIVAGAF(�AC�;AB{AA��AA��AAdZA@ȴA<ȴA:bNA8�HA7�^A77LA6I�A5��A4��A3��A2��A1&�A0E�A.~�A-��A,ȴA+�A*�+A)��A(�+A'A&�/A%��A$jA#�A"�A"�+A!��A z�A��A�FA"�A��AM�AO�A�+AI�A�A�\A�jA�A|�A1'A��AVA�
AffA  A��A�A9XA��AXA
=A$�A
�`A	��A��A(�AC�A=qA�A��A^5A�AA Z@�v�@�V@�1'@���@�"�@���@��@�G�@�1'@��#@��@���@��;@���@띲@�v�@��@�O�@��@�\)@�!@�E�@�@�@�^5@�A�@ڸR@��@��@��`@ج@׮@և+@��m@��#@���@�A�@� �@ύP@�J@���@�S�@ɺ^@�r�@��m@ư!@�=q@��@š�@�%@Õ�@�-@���@�o@�5?@�V@�(�@�"�@�ȴ@���@���@�&�@��9@��u@���@�o@���@�-@��@� �@��@��@�1@��@�"�@��@���@�{@�V@��`@���@�r�@��\@�E�@��@�?}@���@���@�%@�;d@���@��H@���@�ff@���@���@�/@�/@�/@�p�@�O�@��@�bN@���@�9X@�\)@�v�@���@�+@��+@��@��@�{@�{@�{@��@�&�@���@��9@�I�@�ƨ@��F@��y@�v�@�ff@�^5@�M�@�-@���@��7@�%@�Ĝ@��@���@�z�@�b@�|�@��\@���@��T@���@���@�X@�hs@��h@��h@�x�@�x�@�?}@�X@��-@���@�7L@�?}@�x�@�7L@�&�@��@�%@��`@�%@��@�`B@�@��@�X@�O�@�G�@��@��@�?}@�/@��j@�r�@�j@�Q�@�  @��F@�|�@�\)@�+@��y@��R@�~�@�n�@�E�@��#@�x�@�7L@���@���@�(�@���@���@��P@�l�@�K�@��@�@��y@��!@�~�@�V@��@��h@�%@��`@��9@�j@�Q�@�1'@��@��;@���@���@�|�@�o@�~�@�V@�5?@��@���@��h@�?}@��/@���@�j@�I�@��m@�|�@�;d@���@���@�~�@�^5@�=q@�-@�J@��#@��^@���@��7@�hs@�G�@�/@��/@�A�@��@�;d@��@�o@��@�ȴ@��+@�-@���@� �@�(�@vV@jn�@d�@]�-@TZ@L�/@H�@Cƨ@>@6��@/\)@)�#@!��@@+@33@�y@(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A�ffA�hsA�bNA�bNA�jA�v�A�z�A�|�A�x�A�|�A�|�AƃAƃA�|�A�p�A�\)A�C�A��Ař�A�33A���A���A�z�A��#AÃA��A���A��!A��\A�dZA�33A��TA���A�9XA��A���A�K�A��A���A��jA��DA�C�A���A�?}A�bA���A��FA�M�A��jA�E�A��;A��jA�S�A��
A�I�A�"�A�p�A���A��hA�bNA�&�A���A��A�r�A���A�l�A�G�A��A�ZA���A�hsA�oA�33A���A�z�A���A�?}A�ĜA���A�M�A���A�33A�A���A��9A���A�A�A��A��PA�~�A�I�A��A�dZA�p�A���A��^A��A��A�bNA��A���A���A��#A���A���A�Q�A��DA�/A�$�A�A�A���A���A���A� �A��+A��-A��A��A��wA�A�dZA��A��\A���A�ffA�ZA�G�A��;A���A~��A|I�A|Ax�/Av��At1'Ar-Ap�Ao�An��Am�wAkp�Ah�Af�\Ae�-Ad�AcƨAb�Ab��AbJA`�9A]x�AY�PAX�`AU�;AS�PAO�;AM�AK\)AIVAGAF(�AC�;AB{AA��AA��AAdZA@ȴA<ȴA:bNA8�HA7�^A77LA6I�A5��A4��A3��A2��A1&�A0E�A.~�A-��A,ȴA+�A*�+A)��A(�+A'A&�/A%��A$jA#�A"�A"�+A!��A z�A��A�FA"�A��AM�AO�A�+AI�A�A�\A�jA�A|�A1'A��AVA�
AffA  A��A�A9XA��AXA
=A$�A
�`A	��A��A(�AC�A=qA�A��A^5A�AA Z@�v�@�V@�1'@���@�"�@���@��@�G�@�1'@��#@��@���@��;@���@띲@�v�@��@�O�@��@�\)@�!@�E�@�@�@�^5@�A�@ڸR@��@��@��`@ج@׮@և+@��m@��#@���@�A�@� �@ύP@�J@���@�S�@ɺ^@�r�@��m@ư!@�=q@��@š�@�%@Õ�@�-@���@�o@�5?@�V@�(�@�"�@�ȴ@���@���@�&�@��9@��u@���@�o@���@�-@��@� �@��@��@�1@��@�"�@��@���@�{@�V@��`@���@�r�@��\@�E�@��@�?}@���@���@�%@�;d@���@��H@���@�ff@���@���@�/@�/@�/@�p�@�O�@��@�bN@���@�9X@�\)@�v�@���@�+@��+@��@��@�{@�{@�{@��@�&�@���@��9@�I�@�ƨ@��F@��y@�v�@�ff@�^5@�M�@�-@���@��7@�%@�Ĝ@��@���@�z�@�b@�|�@��\@���@��T@���@���@�X@�hs@��h@��h@�x�@�x�@�?}@�X@��-@���@�7L@�?}@�x�@�7L@�&�@��@�%@��`@�%@��@�`B@�@��@�X@�O�@�G�@��@��@�?}@�/@��j@�r�@�j@�Q�@�  @��F@�|�@�\)@�+@��y@��R@�~�@�n�@�E�@��#@�x�@�7L@���@���@�(�@���@���@��P@�l�@�K�@��@�@��y@��!@�~�@�V@��@��h@�%@��`@��9@�j@�Q�@�1'@��@��;@���@���@�|�@�o@�~�@�V@�5?@��@���@��h@�?}@��/@���@�j@�I�@��m@�|�@�;d@���@���@�~�@�^5@�=q@�-@�J@��#@��^@���@��7@�hs@�G�@�/@��/@�A�@��@�;d@��@�o@��@�ȴ@��+@�-G�O�@� �@�(�@vV@jn�@d�@]�-@TZ@L�/@H�@Cƨ@>@6��@/\)@)�#@!��@@+@33@�y@(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBP�BP�BO�BP�BP�BN�BN�BN�BO�BN�BN�BN�BN�BN�BN�BO�BN�BXBcTBk�Br�Bw�B~�B�B�%B�DB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�JB� By�Bv�Bs�BiyBVBI�BE�B;dB5?B/B%�B�BVB
=BB�B�B�B��B��B��B��B��BĜB�!B��B�7Bt�BbNBVBB�B/B�B��B��B�B�B��BȴBŢB�qB��B�7Bs�BcTBJ�B6FB%�BJB
��B
�fB
�)B
��B
ĜB
�9B
��B
��B
�B
r�B
n�B
m�B
jB
W
B
G�B
6FB
%�B
 �B
DB	��B	�fB	�
B	��B	ƨB	�wB	�?B	��B	�hB	�+B	�B	|�B	u�B	p�B	n�B	iyB	`BB	M�B	:^B	5?B	%�B	�B	B��B�B�`B�;B�B��B��B��B��BɺBĜB�dB�LB�3B�!B�B�B�B��B��B��B��B��B��B��B��B�{B�oB�bB�VB�JB�7B�%B�B�B� B~�B}�B~�B}�B{�By�Bw�Bv�Bu�Bu�Bs�Bq�Bo�Bo�Bm�Bl�BjBiyBhsBffBffBe`Be`BdZBdZBcTBcTBbNBaHB`BB_;B_;B^5B]/B[#B\)B\)B[#BZBZB\)B_;BdZBffBffBffBffBffBffBe`BbNB`BB^5B]/B_;BbNBdZBdZBdZBdZBffBhsBgmBffBhsBjBjBjBl�Bm�Bm�Bm�Bn�Bo�Bp�Bp�Bq�Br�Bt�Bv�Bw�Bx�Bz�B|�B� B�B�B�%B�1B�=B�=B�DB�\B�oB��B��B��B��B��B��B��B��B��B��B�9B�3B�-B�-B�'B�9B�LB�RB�XB�XB�^B�dB�qB�wB��B��B��B��B�B�ZB�ZB�TB�BB�5B�NB�ZB�B�B�B�B�B��B��B��B	B		7B	bB	bB	bB	oB	�B	�B	�B	�B	!�B	&�B	'�B	(�B	+B	+B	+B	+B	,B	/B	0!B	0!B	1'B	5?B	9XB	@�B	B�B	C�B	D�B	E�B	J�B	L�B	L�B	M�B	N�B	O�B	O�B	P�B	S�B	VB	\)B	_;B	`BB	bNB	e`B	ffB	gmB	hsB	jB	l�B	o�B	o�B	r�B	w�B	|�B	~�B	�B	�+B	�1B	�1B	�=B	�JB	�PB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�?B	�FB	�XB	�dB	�qB	�wB	�}B	��B	��B	B	ÖB	ĜB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�)B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�ZB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
PB
�B
!�B
)�B
/B
5?B
<jB
>wB
B�B
G�B
M�B
S�B
YB
`BB
gmB
k�B
o�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   BP�BP�BO�BP�BP�BN�BN�BN�BO�BN�BN�BN�BN�BN�BN�BO�BN�BXBc^Bk�Br�Bw�BB�B�0B�MB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�VB�
By�Bv�Bs�Bi�BVBI�BE�B;hB5EB/#B%�B�B^B
CBB�B�B�	B�B�B��B��B��BģB�%B��B�=Bt�BbVBV	BB�B/!B�B��B��B�B�B��BȸBŨB�uB��B�<Bs�Bc\BJ�B6JB%�BPB
��B
�pB
�.B
��B
ĤB
�BB
��B
��B
�B
r�B
n�B
m�B
j�B
WB
G�B
6OB
%�B
 �B
SB	��B	�tB	�B	��B	ƸB	��B	�NB	��B	�yB	�;B	�B	}B	u�B	p�B	n�B	i�B	`TB	M�B	:sB	5RB	%�B	�B	0B��B�B�vB�UB�/B�B��B��B��B��BĵB�B�gB�MB�<B�6B�0B�"B�B�B��B��B��B��B��B��B��B��B�~B�pB�fB�SB�AB�-B�'B�BB~BB~B|By�Bw�Bv�Bu�Bu�Bs�Bq�Bo�Bo�Bm�Bl�Bj�Bi�Bh�Bf�Bf�Be|Be}BduBdvBcqBcrBblBaeB`aB_YB_XB^QB]JB[DB\FB\FB[@BZ:BZ:B\EB_YBduBf�Bf�Bf�Bf�Bf�Bf�Be~BbiB`bB^SB]MB_YBbjBdwBduBdwBdwBf�Bh�Bg�Bf�Bh�Bj�Bj�Bj�Bl�Bm�Bm�Bm�Bn�Bo�Bp�Bp�Bq�Br�Bt�Bv�Bw�Bx�Bz�B}B�B�#B�3B�@B�LB�XB�WB�_B�yB��B��B��B��B��B��B��B��B��B��B�	B�RB�LB�GB�GB�?B�TB�eB�jB�mB�pB�zB�{B��B��B��B��B�	B�B�&B�qB�sB�jB�YB�NB�gB�pB�B�B�B�B��B��B��B�B	4B		MB	vB	wB	vB	�B	�B	�B	�B	�B	!�B	&�B	(B	)B	+B	+B	+B	+B	,B	/-B	03B	04B	1;B	5QB	9kB	@�B	B�B	C�B	D�B	E�B	J�B	L�B	L�B	M�B	N�B	O�B	O�B	P�B	TB	VB	\;B	_LB	`SB	b^B	esB	fwB	g~B	h�B	j�B	l�B	o�B	o�B	r�B	w�B	|�B	B	�$B	�:B	�AB	�AB	�NB	�YB	�aB	�nB	�yB	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�5B	�JB	�NB	�UB	�fB	�wB	��B	��B	��B	��B	��B	B	æB	īB	ĬB	ƶB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�,B	�9B	�>B	�DB	�IB	�GB	�QB	�UB	�VB	�[B	�]B	�bB	�hB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� G�O�B
(B
^B
�B
!�B
*	B
/(B
5KB
<tB
>B
B�B
G�B
M�B
TB
Y B
`LB
guB
k�B
o�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214312016053112143120160531121431  AO  ARCAADJP                                                                    20140721230527    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230527  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230527  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121431  IP                  G�O�G�O�G�O�                
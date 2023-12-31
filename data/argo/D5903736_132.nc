CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-04T10:15:44Z AOML 3.0 creation; 2016-05-31T19:14:46Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151204101544  20160531121446  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_132                   2C  D   APEX                            5368                            041511                          846 @׃�$�˅1   @׃��DK�@4"��`B�d_dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�33B�  B�  B�  B�  B�  B���B�  B�  B���B�  B���B�  B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D�  D�I�D��3D��fD�3D�6fD�� D��3D�  D�FfD���D��3D�	�D�FfD�c3D�ٚD��D�I�D� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�G�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB���B���B�ǮB�ǮB�ǮB�ǮB�ǮB��{B�ǮB�ǮB�aHB�ǮB��{B�ǮB��{BÔ{Bǔ{B˔{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D r�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt�\Dy��D��{D�FD��D���D���D�2�D�|{D�ϮD��{D�B�D��HDǿ�D�D�B�D�_�D��D�	HD�FD�|{D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A� �A� �A��A��A�oA��A�"�A�$�A�$�A�&�A�(�A�(�A�&�A�&�A�$�A�"�A��A��A���A���A֩�A֕�A։7A�z�A�r�A�l�A�jA�jA�jA�jA�l�A�z�Aև+A֏\A֏\A֏\AօA�~�A�v�A�r�A�A�9XA�7LAҟ�A�l�A�?}A�"�A��
A��A���A���A���A�A��7A��A�jA�1A��;A�&�A��HA�ZA�ffA��A�7LA���A���A�&�A��`A�r�A�Q�A�G�A���A��wA���A���A�A��A�S�A���A�S�A�~�A�33A���A�VA�VA��HA��jA�S�A���A��9A�VA��PA��A��`A���A�1A�O�A�`BA�jA���A�z�A��A�|�A��Ap�AzĜAx9XAu&�Arz�Aq�Apv�Aot�AnbNAl{Ai&�Afv�Ad��AbVA`ZA^5?A\�RA[�wAY�AW;dAUx�ATQ�AS�AQ�PAPM�AO�AN�AM��AM&�AL^5AJ�AG��AE�AD�9ACoAA�hA@-A>�yA>bNA=
=A<n�A;��A;%A:=qA9�A8jA5�
A4��A4{A2�9A1��A0z�A/��A.�+A-oA,��A+�A+�A*bA(ĜA&�A&1A%"�A$ �A#��A"�!A!p�A �DA�#At�A��A��A��A�HA^5A�AZA$�A��AQ�A��AM�A��AhsA�TA�`A��A �A�hA�uA
�RA	ƨA	/A�uAVA�A1A�mA&�A(�A��A��AJA�PA ��A jA A�@��@���@���@�~�@�=q@�K�@�ff@��^@�1@�\@�`B@�@�A�@� �@�P@��y@�&�@�R@�O�@��@��`@�~�@��#@���@��H@�$�@��@�(�@���@�hs@��@�bN@��y@��@�I�@���@ёh@��/@��m@Χ�@�hs@��/@���@�l�@�"�@�~�@���@�O�@���@ț�@�1'@Ǿw@�K�@ƸR@ź^@�%@�(�@���@�x�@���@�t�@�v�@�n�@�v�@�@��h@���@���@���@�p�@�G�@�j@��@�K�@��-@��h@���@�O�@��/@�(�@��w@��@���@��@���@���@�ff@���@�p�@��@�z�@��m@��w@���@�l�@��H@�^5@�5?@��@�J@��T@��^@��@�O�@�/@���@��D@�Q�@�  @��F@��P@�S�@��y@��\@�V@��@�@�hs@�&�@��/@�Ĝ@��D@��@�|�@�"�@��!@�v�@�$�@��#@���@�/@��D@�1'@��m@�33@�ȴ@��\@�n�@�J@�G�@�Ĝ@��@�1'@�1@��@��w@��@��P@�|�@�C�@�o@���@�ȴ@�~�@��#@�p�@�G�@�?}@�&�@��`@��j@�r�@��@���@��w@���@���@��@�C�@���@���@�v�@�V@�M�@��@��h@�X@�7L@��@���@�I�@��@���@�dZ@�;d@�C�@�33@�o@��R@�v�@�J@��@��#@��-@�hs@�?}@���@��D@�r�@�j@�j@�Z@�A�@��@��;@��@�;d@��+@���@���@��h@�X@�?}@�G�@�X@���@���@�I�@��@��P@���@�ȴ@�ff@�J@���@�?}@��j@��@�9X@���@�\)@�33@�"�@�o@��R@�E�@��@���@��@��#@��T@�J@�@��@��^@��7@�x�@�7L@��`@���@�r�@�A�@���@��F@�\)@�o@��\@���@���@���@���@�?}@��u@�Q�@� �@��@� �@�b@� �@�1@���@���@�S�@���@���@�-@��T@��h@��P@��@v$�@l(�@b�!@X�9@Q7L@H�`@C�F@=p�@6{@.ff@)hs@%p�@!7L@j@1'@1@�@C�@bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A� �A� �A��A��A�oA��A�"�A�$�A�$�A�&�A�(�A�(�A�&�A�&�A�$�A�"�A��A��A���A���A֩�A֕�A։7A�z�A�r�A�l�A�jA�jA�jA�jA�l�A�z�Aև+A֏\A֏\A֏\AօA�~�A�v�A�r�A�A�9XA�7LAҟ�A�l�A�?}A�"�A��
A��A���A���A���A�A��7A��A�jA�1A��;A�&�A��HA�ZA�ffA��A�7LA���A���A�&�A��`A�r�A�Q�A�G�A���A��wA���A���A�A��A�S�A���A�S�A�~�A�33A���A�VA�VA��HA��jA�S�A���A��9A�VA��PA��A��`A���A�1A�O�A�`BA�jA���A�z�A��A�|�A��Ap�AzĜAx9XAu&�Arz�Aq�Apv�Aot�AnbNAl{Ai&�Afv�Ad��AbVA`ZA^5?A\�RA[�wAY�AW;dAUx�ATQ�AS�AQ�PAPM�AO�AN�AM��AM&�AL^5AJ�AG��AE�AD�9ACoAA�hA@-A>�yA>bNA=
=A<n�A;��A;%A:=qA9�A8jA5�
A4��A4{A2�9A1��A0z�A/��A.�+A-oA,��A+�A+�A*bA(ĜA&�A&1A%"�A$ �A#��A"�!A!p�A �DA�#At�A��A��A��A�HA^5A�AZA$�A��AQ�A��AM�A��AhsA�TA�`A��A �A�hA�uA
�RA	ƨA	/A�uAVA�A1A�mA&�A(�A��A��AJA�PA ��A jA A�@��@���@���@�~�@�=q@�K�@�ff@��^@�1@�\@�`B@�@�A�@� �@�P@��y@�&�@�R@�O�@��@��`@�~�@��#@���@��H@�$�@��@�(�@���@�hs@��@�bN@��y@��@�I�@���@ёh@��/@��m@Χ�@�hs@��/@���@�l�@�"�@�~�@���@�O�@���@ț�@�1'@Ǿw@�K�@ƸR@ź^@�%@�(�@���@�x�@���@�t�@�v�@�n�@�v�@�@��h@���@���@���@�p�@�G�@�j@��@�K�@��-@��h@���@�O�@��/@�(�@��w@��@���@��@���@���@�ff@���@�p�@��@�z�@��m@��w@���@�l�@��H@�^5@�5?@��@�J@��T@��^@��@�O�@�/@���@��D@�Q�@�  @��F@��P@�S�@��y@��\@�V@��@�@�hs@�&�@��/@�Ĝ@��D@��@�|�@�"�@��!@�v�@�$�@��#@���@�/@��D@�1'@��m@�33@�ȴ@��\@�n�@�J@�G�@�Ĝ@��@�1'@�1@��@��w@��@��P@�|�@�C�@�o@���@�ȴ@�~�@��#@�p�@�G�@�?}@�&�@��`@��j@�r�@��@���@��w@���@���@��@�C�@���@���@�v�@�V@�M�@��@��h@�X@�7L@��@���@�I�@��@���@�dZ@�;d@�C�@�33@�o@��R@�v�@�J@��@��#@��-@�hs@�?}@���@��D@�r�@�j@�j@�Z@�A�@��@��;@��@�;d@��+@���@���@��h@�X@�?}@�G�@�X@���@���@�I�@��@��P@���@�ȴ@�ff@�J@���@�?}@��j@��@�9X@���@�\)@�33@�"�@�o@��R@�E�@��@���@��@��#@��T@�J@�@��@��^@��7@�x�@�7L@��`@���@�r�@�A�@���@��F@�\)@�o@��\@���@���@���@���@�?}@��u@�Q�@� �@��@� �@�b@� �@�1@���@���@�S�@���@���@�-@��T@��h@��P@��@v$�@l(�@b�!@X�9@Q7L@H�`@C�F@=p�@6{@.ff@)hs@%p�@!7L@j@1'@1@�@C�@bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBffBffBffBffBffBffBffBffBffBffBffBffBhsBhsBiyBjBl�Bq�B� B��B��B��B��B�B�B�3B�9B�9B�?B�FB�FB�dB�}BBÖBÖB��B��B�wB�}Bp�B�B%�BG�BO�BcTBm�BjBn�BhsBbNB]/BT�BG�B;dB-B'�B%�B�BDB��B�B�mB�BÖB�9B�-B��B��B��B��B��B�{B�Bx�BhsBR�B�B��B�B�B�yB�`B��B��B�dB�LB�B��B�B{�Bu�Bm�B[#BG�B;dB/B�BbB
��B
�B
�B
�jB
��B
�B
]/B
F�B
.B
�B
\B
	7B
B	��B	�fB	��B	�jB	�B	��B	�JB	}�B	t�B	m�B	`BB	T�B	K�B	D�B	B�B	7LB	/B	,B	(�B	"�B	�B	�B	JB	B��B��B�B�B�ZB�;B�5B�B�B�B�
B��B��B��BŢBB��B�jB�RB�9B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�bB�VB�PB�PB�JB�=B�=B�+B�B�B�B�B�B}�Bz�Bx�B{�B|�Bx�Bs�Bp�Bp�Bk�BiyBm�Bn�Bm�Bl�BiyBgmBdZBgmBhsBgmBgmBffBhsBm�Bm�Bm�Bu�Br�Br�Bt�Bw�By�Bz�By�Bz�By�By�Bw�Bu�Bv�Bw�Bw�Bn�Bm�Bl�Bp�Bs�Bs�Bt�Bt�Bv�Bu�Bw�B{�B}�B�B�+B�1B�bB�oB�{B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�3B�9B�3B�3B�9B�RB�jB��BÖBǮB��B��B�B�B�B�;B�NB�NB�fB�fB�fB�B�B�B��B��B��B��B	B	B	1B	1B	1B	
=B	VB	bB	uB	�B	�B	�B	�B	�B	%�B	'�B	)�B	)�B	+B	,B	,B	-B	0!B	33B	49B	5?B	7LB	8RB	;dB	<jB	>wB	B�B	E�B	G�B	J�B	K�B	N�B	P�B	R�B	R�B	S�B	T�B	YB	]/B	_;B	bNB	dZB	hsB	jB	l�B	n�B	p�B	s�B	w�B	y�B	{�B	|�B	� B	�B	�+B	�1B	�=B	�DB	�DB	�VB	�\B	�bB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�9B	�?B	�FB	�FB	�XB	�^B	�dB	�dB	�dB	�qB	�}B	��B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�/B	�;B	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
  B
  B
  B
  B
  B	��B
  B
  B
  B
B
B
B
B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
JB
�B
�B
%�B
-B
1'B
8RB
?}B
G�B
J�B
P�B
VB
\)B
cTB
gmB
l�B
o�B
s�B
w�B
{�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BfpBfrBfpBfpBfpBfpBfpBfpBfnBfpBfpBfpBh}Bh{Bi�Bj�Bl�Bq�B�
B��B��B��B��B�B�'B�;B�BB�BB�GB�PB�PB�kB��BBßBßB��B��B��B��Bp�B�B%�BG�BO�BcbBm�Bj�Bn�Bh}BbZB]<BU
BG�B;qB-B'�B%�B�BKB��B�B�zB�#BáB�EB�5B�B��B��B��B��B��B�'Bx�Bh~BR�B�B��B�B�B�B�kB�B��B�qB�XB�B��B�)B{�Bu�Bm�B[,BG�B;nB/(B�BpB
��B
�B
�#B
�xB
�B
�'B
]>B
F�B
.%B
�B
pB
	KB
B	��B	�{B	�B	�B	�B	��B	�bB	~B	t�B	m�B	`[B	UB	K�B	D�B	B�B	7fB	/8B	,$B	)B	"�B	�B	�B	fB	+B��B��B��B�B�yB�ZB�RB�;B�6B�5B�)B�B��B��B��B¯B��B��B�rB�ZB�NB�BB�.B�*B�B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�zB�tB�uB�kB�aB�`B�PB�@B�;B�:B�5B�+B~B{Bx�B|B}Bx�Bs�Bp�Bp�Bk�Bi�Bm�Bn�Bm�Bl�Bi�Bg�Bd|Bg�Bh�Bg�Bg�Bf�Bh�Bm�Bm�Bm�Bu�Br�Br�Bt�Bw�Bz B{By�B{Bz BzBw�Bu�Bv�Bw�Bw�Bn�Bm�Bl�Bp�Bs�Bs�Bt�Bt�Bv�Bu�Bw�B|
B~B�@B�MB�TB��B��B��B��B��B��B��B��B��B�B�B�)B�/B�5B�;B�BB�GB�PB�YB�QB�TB�XB�qB��B��B÷B��B��B��B�$B�<B�>B�ZB�mB�nB�B�B�B�B�B��B��B��B�B�B	%B	0B	PB	NB	PB	
ZB	sB	�B	�B	�B	�B	�B	�B	�B	&B	(B	*B	*B	+B	,#B	,"B	-+B	0<B	3MB	4RB	5YB	7fB	8nB	;�B	<�B	>�B	B�B	E�B	G�B	J�B	K�B	N�B	P�B	SB	SB	TB	UB	Y1B	]GB	_VB	bfB	dtB	h�B	j�B	l�B	n�B	p�B	s�B	w�B	y�B	| B	}B	�B	�*B	�CB	�JB	�VB	�\B	�]B	�nB	�sB	�}B	�yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�,B	�,B	�3B	�7B	�MB	�OB	�SB	�\B	�\B	�nB	�sB	�|B	�}B	�}B	��B	��B	��B	¥B	íB	ıB	ŷB	ŸB	ŹB	ƽB	ƿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�B	�B	�!B	�,B	�4B	�EB	�QB	�WB	�^B	�`B	�jB	�hB	�pB	�pB	�rB	�oB	�uB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B
 B
 B
B
B
 B
 B
 B
 B
 B	�B
 B
 B
 B
B
!B
*B
6B
AB
GB
GB
HB
GB
GB
	NB
	KB
	KB
_B
�B
�B
%�B
-#B
1:B
8hB
?�B
G�B
J�B
P�B
VB
\;B
cfB
g�B
l�B
o�B
s�B
w�B
{�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214462016053112144620160531121446  AO  ARCAADJP                                                                    20151204101544    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151204101544  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151204101544  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121446  IP                  G�O�G�O�G�O�                
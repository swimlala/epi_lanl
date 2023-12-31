CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:26Z AOML 3.0 creation; 2016-05-31T19:14:31Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230526  20160531121431  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               'A   AO  4051_7090_039                   2C  D   APEX                            5368                            041511                          846 @֕g�?�1   @֕g�� @4�x����d�~��"�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    'A   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy� D�fD�I�D���D���D�	�D�6fD�l�D�ٚD�3D�I�D���D��fD� D�C3D�s3D�� D�3D�9�D�p D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @8��@x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C�qC��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	�\D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��DtX�Dyx�D��D�FD��D��HD�D�2�D�iHD��D��D�FD��D���D�{D�?�D�o�D��{D��D�6D�l{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�VA�1A�JA�bA�bA�bA�oA�oA�oA�oA�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�~�A��A���AǛ�A�oAŉ7A�"�AËDA�9XA���A�\)A�7LA�bA���A�I�A�1A���A��DA�`BA�XA�S�A�M�A�G�A�C�A�A�A�5?A��A�z�A��!A�A�A�JA��mA�(�A���A��`A�+A�VA��#A�p�A�oA�;dA���A��A�G�A��A�ĜA��^A�5?A��A�(�A��A�%A��/A��A��A�G�A��+A�VA��TA�VA���A�G�A���A��A�z�A��\A�A�E�A���A���A��7A��
A�^5A��^A��/A� �A�VA��jA���A�|�A�+A��A�ȴA�O�A��;A�t�A��9A��A���A��A�"�A��A�JA~�\A|��Az��Axn�Aw
=Au"�ArȴAq&�AoAl(�Ajz�Af�AdA�A_�mA]�#AY�AW"�AVn�AU�AP��AO��AN��AL��AKS�AJĜAI��AHn�AHbAG|�AGS�AEƨAD=qAC��AB�9A?|�A<��A;�-A:v�A8�jA7+A5ƨA3��A1�A0r�A.r�A.A,�A+&�A*�`A*1'A'��A&��A&A%
=A${A#�A#x�A"��A"ZA!�A!�7A!�A!l�A!?}A bNA��Az�AG�AS�A��A��A �A�wA�`A�A��A  A�Al�A�AbA�A{A�
A��Ax�AVA
bNA	x�A��AE�A/A��A��A��A�-A��@�K�@�I�@�C�@��^@�(�@�33@�E�@�1'@�{@�l�@��@�h@��y@�&�@�C�@�^5@�7L@�;d@߅@��/@�A�@ۮ@�E�@���@�b@��@ԃ@�@д9@�Q�@�ƨ@ύP@�S�@�@·+@�{@Ͳ-@�X@��`@˶F@�@�~�@�$�@���@�hs@�Ĝ@�bN@���@�^5@��@���@�9X@�l�@�33@�@��@�V@��u@�9X@�ƨ@�33@���@�~�@��#@�&�@� �@�o@��@�ƨ@�~�@��@���@��7@���@��H@��7@��h@���@���@�p�@�7L@���@�Z@�
=@���@�=q@���@�hs@�O�@���@�Q�@�1@�ƨ@��@���@���@�Z@�Q�@��-@�=q@��@�Ĝ@�A�@�K�@��!@��@���@�(�@�@���@�  @��@�$�@��#@��@�b@�ƨ@��@��@���@�ff@�=q@�-@��@��T@��@�{@�5?@�M�@�=q@�$�@���@�?}@�&�@��@��/@��/@��@��@���@�K�@�;d@�;d@�;d@�C�@�S�@�dZ@���@���@�t�@�S�@�33@�S�@�t�@��@�o@��!@���@��@�o@�"�@�
=@��y@��@��7@��@�J@��7@�p�@��h@��-@�@��@�hs@�O�@�?}@��@��@���@���@�Q�@�I�@�A�@�9X@�  @���@���@���@��@��y@��@��R@�-@�-@�5?@�$�@�J@��-@�X@��@��@��`@�Ĝ@��j@�z�@�1'@��;@�ƨ@�l�@�+@�o@�@��y@��!@�ff@�=q@��@���@�X@�&�@�V@���@���@��@�j@�A�@��@�  @��@��
@��F@�|�@�S�@�+@�o@��@��R@�^5@�@��h@�X@�/@��@���@���@���@�z�@�Z@�  @��F@��@���@�|�@�K�@�@���@��+@�~�@�v�@�^5@�5?@�-@�{@��#@���@��-@�hs@�&�@��@��@��!@}�h@t�@k�@c@Yx�@Q�@G�@@A�@9��@2�H@,�j@&ȴ@!�^@9X@�;@�D@��@�@	X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�bA�VA�1A�JA�bA�bA�bA�oA�oA�oA�oA�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�~�A��A���AǛ�A�oAŉ7A�"�AËDA�9XA���A�\)A�7LA�bA���A�I�A�1A���A��DA�`BA�XA�S�A�M�A�G�A�C�A�A�A�5?A��A�z�A��!A�A�A�JA��mA�(�A���A��`A�+A�VA��#A�p�A�oA�;dA���A��A�G�A��A�ĜA��^A�5?A��A�(�A��A�%A��/A��A��A�G�A��+A�VA��TA�VA���A�G�A���A��A�z�A��\A�A�E�A���A���A��7A��
A�^5A��^A��/A� �A�VA��jA���A�|�A�+A��A�ȴA�O�A��;A�t�A��9A��A���A��A�"�A��A�JA~�\A|��Az��Axn�Aw
=Au"�ArȴAq&�AoAl(�Ajz�Af�AdA�A_�mA]�#AY�AW"�AVn�AU�AP��AO��AN��AL��AKS�AJĜAI��AHn�AHbAG|�AGS�AEƨAD=qAC��AB�9A?|�A<��A;�-A:v�A8�jA7+A5ƨA3��A1�A0r�A.r�A.A,�A+&�A*�`A*1'A'��A&��A&A%
=A${A#�A#x�A"��A"ZA!�A!�7A!�A!l�A!?}A bNA��Az�AG�AS�A��A��A �A�wA�`A�A��A  A�Al�A�AbA�A{A�
A��Ax�AVA
bNA	x�A��AE�A/A��A��A��A�-A��@�K�@�I�@�C�@��^@�(�@�33@�E�@�1'@�{@�l�@��@�h@��y@�&�@�C�@�^5@�7L@�;d@߅@��/@�A�@ۮ@�E�@���@�b@��@ԃ@�@д9@�Q�@�ƨ@ύP@�S�@�@·+@�{@Ͳ-@�X@��`@˶F@�@�~�@�$�@���@�hs@�Ĝ@�bN@���@�^5@��@���@�9X@�l�@�33@�@��@�V@��u@�9X@�ƨ@�33@���@�~�@��#@�&�@� �@�o@��@�ƨ@�~�@��@���@��7@���@��H@��7@��h@���@���@�p�@�7L@���@�Z@�
=@���@�=q@���@�hs@�O�@���@�Q�@�1@�ƨ@��@���@���@�Z@�Q�@��-@�=q@��@�Ĝ@�A�@�K�@��!@��@���@�(�@�@���@�  @��@�$�@��#@��@�b@�ƨ@��@��@���@�ff@�=q@�-@��@��T@��@�{@�5?@�M�@�=q@�$�@���@�?}@�&�@��@��/@��/@��@��@���@�K�@�;d@�;d@�;d@�C�@�S�@�dZ@���@���@�t�@�S�@�33@�S�@�t�@��@�o@��!@���@��@�o@�"�@�
=@��y@��@��7@��@�J@��7@�p�@��h@��-@�@��@�hs@�O�@�?}@��@��@���@���@�Q�@�I�@�A�@�9X@�  @���@���@���@��@��y@��@��R@�-@�-@�5?@�$�@�J@��-@�X@��@��@��`@�Ĝ@��j@�z�@�1'@��;@�ƨ@�l�@�+@�o@�@��y@��!@�ff@�=q@��@���@�X@�&�@�V@���@���@��@�j@�A�@��@�  @��@��
@��F@�|�@�S�@�+@�o@��@��R@�^5@�@��h@�X@�/@��@���@���@���@�z�@�Z@�  @��F@��@���@�|�@�K�@�@���@��+@�~�@�v�@�^5@�5?@�-@�{@��#@���@��-@�hs@�&�G�O�@��@��!@}�h@t�@k�@c@Yx�@Q�@G�@@A�@9��@2�H@,�j@&ȴ@!�^@9X@�;@�D@��@�@	X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBBBBB��BBBBBBB��BBBBBBBBBBBBB��BÖB��B��B�B�)B�fB%B1'BF�BQ�B`BBk�Bn�Br�Bw�B� B�B�B�%B�+B�+B�+B�+B�+B�+B�+B�%B�%B�B|�Bw�Bt�Br�BiyB[#BM�BE�B<jB6FB1'B+B�B1B  B��B�B��B  BB��B�B�;B�5B�BɺB�RB�B��B��B�JB�Bz�Bu�Bp�Bl�BgmBXB?}B�B
=BB�B�TB�B��B��B�3B��B�uBH�B<jB33BhB
�B
��B
��B
�RB
�B
��B
�B
v�B
gmB
S�B
E�B
8RB
+B
�B

=B	��B	�yB	�B	��B	B	�'B	��B	�PB	|�B	bNB	R�B	A�B	7LB	2-B	+B	�B	�B	\B	B��B��B��B�B�B�B�B�B�mB�`B�;B��B��B��B��BɺBŢBB�dB�'B��B��B��B��B��B��B�oB�\B�VB�JB�DB�=B�=B�7B�1B�+B�1B�1B�1B�1B�+B�%B�B~�B}�By�Bv�Bt�Br�Bp�Bl�BiyBgmBffBffBe`BdZBbNBaHBbNBaHBaHB`BB`BB_;B^5B]/B]/B_;B]/B\)B\)B\)BZBaHBaHB`BB_;BaHBbNBcTBiyBffBe`Bn�Bo�Bq�Bp�Bq�Bp�Bn�Bm�Bm�Bm�Bn�Bn�Bp�Br�Bq�Br�Bp�Bs�Bv�Bv�Bv�Bv�Bv�Bw�Bx�Bx�By�By�By�B}�B� B�B�B�+B�7B�=B�DB�JB�PB�bB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�jB��B��BÖBŢBǮBǮBȴB��B��B��B�
B�
B�
B�B�B�/B�;B�HB�sB�B�B�B�B��B��B	  B	%B	%B	B	B��B��B	  B	
=B	\B	oB	oB	uB	uB	�B	�B	�B	�B	!�B	&�B	(�B	(�B	)�B	-B	2-B	33B	5?B	7LB	;dB	?}B	D�B	E�B	H�B	I�B	J�B	L�B	S�B	YB	]/B	_;B	_;B	_;B	_;B	_;B	`BB	`BB	bNB	e`B	gmB	hsB	hsB	k�B	m�B	n�B	p�B	r�B	s�B	y�B	}�B	�B	�B	�B	�B	�B	�7B	�DB	�=B	�JB	�bB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�3B	�9B	�LB	�XB	�jB	�jB	�qB	�wB	�}B	�}B	��B	ÖB	ĜB	ŢB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�TB	�ZB	�ZB	�fB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
	7B
oB
�B
 �B
'�B
0!B
7LB
?}B
F�B
K�B
Q�B
YB
^5B
cTB
hsB
l�B
o�B
r�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BBBBBBBBB��BBBBBBB��BBBBBBBBBBBBB��BäB��B��B� B�1B�sB-B13BF�BQ�B`OBk�Bn�Br�Bw�B�B� B�*B�2B�<B�:B�:B�<B�<B�:B�<B�5B�6B�)B|�Bw�Bt�Br�Bi�B[/BM�BE�B<uB6SB16B+B�B<B B��B�B��B B%B�B�B�DB�@B�)B��B�]B�$B��B��B�UB�Bz�Bu�Bp�Bl�BgxBXB?�B�B
FBB�B�^B�'B��B��B�=B��B��BH�B<wB3=BvB
�B
�B
��B
�_B
�B
��B
�&B
v�B
g~B
T
B
E�B
8bB
+B
�B

QB	��B	�B	�$B	��B	¦B	�=B	��B	�gB	}B	bgB	SB	A�B	7fB	2GB	+B	�B	�B	xB	;B�B�B��B��B�B�B�B�B�B�}B�XB�B��B��B��B��B��B®B��B�HB�B��B��B��B��B��B��B��B�xB�nB�gB�_B�aB�\B�TB�OB�RB�TB�WB�UB�OB�IB�=BB~By�Bv�Bt�Br�Bp�Bl�Bi�Bg�Bf�Bf�Be�Bd}BbuBalBbsBajBapB`gB`fB__B^XB]QB]RB__B]RB\NB\LB\NBZ@BamBanB`fB_`BamBbrBcxBi�Bf�Be�Bn�Bo�Bq�Bp�Bq�Bp�Bn�Bm�Bm�Bm�Bn�Bn�Bp�Br�Bq�Br�Bp�Bs�Bv�Bv�Bv�Bv�Bv�Bw�Bx�Bx�By�By�Bz B~B�$B�6B�?B�PB�YB�]B�fB�lB�qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�(B�0B�4B�CB�LB��B��B��BôBſB��B��B��B��B�
B�B�)B�(B�)B�.B�;B�OB�XB�gB�B��B��B��B�B��B�B	 B	BB	BB	7B	%B�B��B	 B	
XB	xB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	'B	)B	)B	*B	-*B	2GB	3OB	5[B	7hB	;~B	?�B	D�B	E�B	H�B	I�B	J�B	L�B	TB	Y1B	]GB	_XB	_VB	_VB	_VB	_VB	`YB	`\B	bgB	e{B	g�B	h�B	h�B	k�B	m�B	n�B	p�B	r�B	s�B	y�B	~B	� B	�*B	�+B	�$B	�9B	�QB	�]B	�VB	�eB	�{G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	� B	�!B	�B	�9B	�DB	�JB	�KB	�OB	�cB	�oB	��B	��B	��B	��B	��B	��B	��B	ìB	ĳB	źB	źB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�B	� B	�-B	�4B	�:B	�?B	�DB	�EB	�KB	�KB	�RB	�WB	�XB	�XB	�[B	�_B	�]B	�iB	�oB	�qB	�{B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
B
	KB
�B
�B
 �B
(B
03B
7_B
?�B
F�B
K�B
Q�B
Y)B
^GB
cfB
h�B
l�B
o�B
r�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214312016053112143120160531121431  AO  ARCAADJP                                                                    20140721230526    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230526  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230526  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121431  IP                  G�O�G�O�G�O�                
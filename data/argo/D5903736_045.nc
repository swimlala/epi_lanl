CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:30Z AOML 3.0 creation; 2016-05-31T19:14:32Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230530  20160531121432  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               -A   AO  4051_7090_045                   2C  D   APEX                            5368                            041511                          846 @֤��o'�1   @֤ǉJ��@5XbM��d�n��O�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    -A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D�  D�&fD��3D��3D� D�P D�l�D�� D�fD�0 D���D�ɚD�  D�9�Dڜ�D�� D� D�@ D�l�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @2�]@x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG��BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C�qC��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtl)Dy��D��{D�"�D��D���D�{D�L{D�iHD��{D��D�,{D��HD��D��{D�6DڙHD�{D�{D�<{D�iHD��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A�ƨA���A԰!Aԟ�A�|�A�K�A�1'A��A�VA�  A��A��mA��/A���A���A�ƨA�A���AӺ^AӰ!Aӧ�A�x�A��A�E�A�$�A��A�ZA�XA��HA��A�5?A�A��HAPA��A��#A�ȴA���A�33A�&�A��A�-A��/A�-A��A�
=A�z�A��yA�jA�jA� �A�bA�bNA�VA�l�A��+A�33A�ffA�{A��9A��A��HA�hsA���A�|�A��A��`A�;dA��A���A��;A�C�A�v�A�1'A��A��7A�|�A�
=A�S�A�XA� �A�;dA���A�$�A���A�"�A�I�A���A�9XA��A��A�|�A���A���A��A�9XA��RA���A���A��uA���A�A�/A��9A���A��A�I�A�(�A�5?A~ȴA{�FAy��Ay�At��ApI�Al�9Al�Ak�7Ak;dAj��Ai��Ah�!Af��Ae7LAc��A_��A^=qA\�A\JAZ��AZbAYp�AX�/AX��AW�AV�!AU�TAT-AS&�AP�yAO%AN�/ANALQ�AK��AJ�jAI��AI�AH �AF�`AF9XAEVACAC\)AA�mA@�yA@ffA?`BA=ƨA;XA:�+A9�7A9
=A8-A6ZA5�-A5\)A4ȴA4E�A3�#A333A1�;A0E�A.v�A-��A-p�A,��A, �A+��A+;dA*-A)l�A(�uA($�A'�TA&ĜA%%A"v�A!G�A M�A�#AȴA�
A�`A��A5?A�A��AjA��A��AE�A�^A|�AC�A�jAM�A��A�yA��AQ�A;dAĜA�DA�+AE�A�#A�A
�yA	��A	l�AE�A�#A�/AZA�A �A ��@�dZ@�@���@�ƨ@�V@�r�@�{@��`@��@��@�@�Z@���@�Z@���@���@�33@噚@�  @�dZ@�n�@�ȴ@ܬ@��@���@ف@�?}@���@��@�{@�p�@��@�+@�-@�/@��@��@���@ʟ�@ț�@�|�@��@�v�@ŉ7@Ĭ@��@�33@��h@�?}@��@���@��@���@�J@���@�5?@��@�@��@��`@��@�o@�5?@�X@��`@�r�@�S�@���@��D@�  @�l�@�V@��`@�K�@�+@�v�@��@�O�@�%@��@��@��\@�$�@�@�x�@���@�A�@�1@��m@���@�ƨ@��w@��F@��@�K�@�M�@���@�r�@�Q�@�(�@�b@�  @��@���@���@�~�@�E�@�$�@�@��@���@���@�(�@�b@�1@��@��m@���@���@�dZ@��@��@��\@�@��-@��@�&�@���@��u@�1@���@���@�|�@�dZ@�"�@��R@�V@�{@��@���@�p�@�X@�&�@���@��/@��j@�Q�@�Ĝ@�hs@���@�ff@�^5@�/@���@�Ĝ@��j@��D@���@���@���@��@��-@��#@��@�-@�5?@�^5@�E�@��#@���@���@���@��@�p�@�`B@�X@�p�@��@�G�@���@�Q�@��
@�S�@�;d@�33@��H@�@��h@�X@�&�@��@��@���@��9@�r�@�1'@��@��F@���@��@�t�@�t�@�dZ@�S�@�+@�o@��@�~�@��@��-@���@���@���@���@��7@�G�@���@�Z@�A�@�1'@��@�  @�ƨ@�C�@��@�ȴ@��!@��+@�M�@�-@�J@���@���@���@��h@�hs@�O�@�7L@�Ĝ@��@�I�@�(�@�  @�l�@��!@�M�@�^5@�M�@�-@��@�x�@�7L@��@���@��@��u@��@�Q�@��h@�9X@xQ�@p��@g|�@\I�@R�@L1@C��@=O�@5�@0�`@+��@%p�@�@�^@5?@�!@l�@�F@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A�ƨA���A԰!Aԟ�A�|�A�K�A�1'A��A�VA�  A��A��mA��/A���A���A�ƨA�A���AӺ^AӰ!Aӧ�A�x�A��A�E�A�$�A��A�ZA�XA��HA��A�5?A�A��HAPA��A��#A�ȴA���A�33A�&�A��A�-A��/A�-A��A�
=A�z�A��yA�jA�jA� �A�bA�bNA�VA�l�A��+A�33A�ffA�{A��9A��A��HA�hsA���A�|�A��A��`A�;dA��A���A��;A�C�A�v�A�1'A��A��7A�|�A�
=A�S�A�XA� �A�;dA���A�$�A���A�"�A�I�A���A�9XA��A��A�|�A���A���A��A�9XA��RA���A���A��uA���A�A�/A��9A���A��A�I�A�(�A�5?A~ȴA{�FAy��Ay�At��ApI�Al�9Al�Ak�7Ak;dAj��Ai��Ah�!Af��Ae7LAc��A_��A^=qA\�A\JAZ��AZbAYp�AX�/AX��AW�AV�!AU�TAT-AS&�AP�yAO%AN�/ANALQ�AK��AJ�jAI��AI�AH �AF�`AF9XAEVACAC\)AA�mA@�yA@ffA?`BA=ƨA;XA:�+A9�7A9
=A8-A6ZA5�-A5\)A4ȴA4E�A3�#A333A1�;A0E�A.v�A-��A-p�A,��A, �A+��A+;dA*-A)l�A(�uA($�A'�TA&ĜA%%A"v�A!G�A M�A�#AȴA�
A�`A��A5?A�A��AjA��A��AE�A�^A|�AC�A�jAM�A��A�yA��AQ�A;dAĜA�DA�+AE�A�#A�A
�yA	��A	l�AE�A�#A�/AZA�A �A ��@�dZ@�@���@�ƨ@�V@�r�@�{@��`@��@��@�@�Z@���@�Z@���@���@�33@噚@�  @�dZ@�n�@�ȴ@ܬ@��@���@ف@�?}@���@��@�{@�p�@��@�+@�-@�/@��@��@���@ʟ�@ț�@�|�@��@�v�@ŉ7@Ĭ@��@�33@��h@�?}@��@���@��@���@�J@���@�5?@��@�@��@��`@��@�o@�5?@�X@��`@�r�@�S�@���@��D@�  @�l�@�V@��`@�K�@�+@�v�@��@�O�@�%@��@��@��\@�$�@�@�x�@���@�A�@�1@��m@���@�ƨ@��w@��F@��@�K�@�M�@���@�r�@�Q�@�(�@�b@�  @��@���@���@�~�@�E�@�$�@�@��@���@���@�(�@�b@�1@��@��m@���@���@�dZ@��@��@��\@�@��-@��@�&�@���@��u@�1@���@���@�|�@�dZ@�"�@��R@�V@�{@��@���@�p�@�X@�&�@���@��/@��j@�Q�@�Ĝ@�hs@���@�ff@�^5@�/@���@�Ĝ@��j@��D@���@���@���@��@��-@��#@��@�-@�5?@�^5@�E�@��#@���@���@���@��@�p�@�`B@�X@�p�@��@�G�@���@�Q�@��
@�S�@�;d@�33@��H@�@��h@�X@�&�@��@��@���@��9@�r�@�1'@��@��F@���@��@�t�@�t�@�dZ@�S�@�+@�o@��@�~�@��@��-@���@���@���@���@��7@�G�@���@�Z@�A�@�1'@��@�  @�ƨ@�C�@��@�ȴ@��!@��+@�M�@�-@�J@���@���@���@��h@�hs@�O�@�7L@�Ĝ@��@�I�@�(�@�  @�l�@��!@�M�@�^5@�M�@�-@��@�x�@�7L@��@���@��@��u@��@�Q�@��h@�9X@xQ�@p��@g|�@\I�@R�@L1@C��@=O�@5�@0�`@+��@%p�@�@�^@5?@�!@l�@�F@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBH�BH�BG�BG�BG�BF�BF�BE�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BE�BE�BE�BF�BF�BF�BJ�Bt�B��B��B��B��B��BffB\)B[#Bu�B�DB�BW
BA�BW
BN�BO�BN�BJ�B<jB'�B%�BD�BR�BO�BQ�BB�B5?B6FB@�B9XB;dBK�B<jB33B49B33B-B%�B"�B�B�BoBoBoB�B�B!�B%�B"�B	7B\BPB�mB��B�LB�B�-B�9B��B��B�PB�Bv�BdZBA�B$�BB�B�B�FB�uBgmBO�BC�B/BDB
��B
�
B
�^B
�!B
��B
��B
�hB
~�B
ZB
E�B
'�B
�B
�B
!�B
VB	�B	�5B	�NB	�ZB	�NB	�BB	�#B	��B	ɺB	�wB	�9B	��B	�{B	�JB	�1B	�B	|�B	y�B	v�B	u�B	t�B	q�B	l�B	e`B	_;B	VB	M�B	K�B	H�B	F�B	E�B	>wB	8RB	9XB	33B	.B	,B	+B	(�B	%�B	�B	�B	�B	{B	
=B	B��B��B��B��B�B�B�B�sB�fB�NB�5B�B��B��B��B��BȴBƨBŢBB�}B�jB�XB�LB�?B�!B��B��B��B��B��B��B�oB�bB�VB�DB�+B�B�B}�Bz�Bw�Bv�Bv�Bt�Bs�Br�Bp�Bo�Bn�Bl�Bk�BjBjBjBiyBgmBe`BdZBcTBaHB^5B^5B^5B\)B\)B[#BZBZBZBZBYBXBW
BXBXBYBYBYBXB\)B[#B]/B]/B]/B]/B\)B[#BYB]/B`BB_;BaHBaHBbNBbNBffBhsBiyBiyBk�Bk�Bl�Bk�Bl�Bl�Bp�Bu�Bw�Bx�Bx�Bz�B{�B|�B~�B�%B�DB�bB�oB��B�{B��B��B��B��B��B��B��B��B�B�FB�dB�jB�wB��BÖBȴB��B��B��B��B�B�)B�/B�5B�HB�NB�TB�mB�B��B��B��B��B	B	B	B	B	B	B	B	B	B	1B	oB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	&�B	&�B	&�B	'�B	(�B	'�B	(�B	)�B	/B	49B	5?B	8RB	9XB	:^B	=qB	>wB	?}B	?}B	?}B	A�B	D�B	F�B	H�B	I�B	O�B	Q�B	T�B	ZB	[#B	\)B	]/B	_;B	hsB	p�B	r�B	x�B	�B	�B	�B	�B	�B	�+B	�JB	�oB	��B	��B	��B	��B	��B	�B	�B	�3B	�9B	�?B	�LB	�XB	�^B	�^B	�dB	�jB	�qB	�wB	��B	��B	ÖB	ǮB	ǮB	ǮB	ƨB	ƨB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�HB	�NB	�HB	�NB	�NB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B

=B
uB
�B
#�B
.B
5?B
>wB
D�B
I�B
N�B
R�B
W
B
ZB
aHB
e`B
hsB
l�B
o�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BH�BH�BG�BG�BG�BF�BF�BE�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BE�BE�BE�BF�BF�BF�BJ�Bt�B��B��B��B��B��BfvB\7B[-Bu�B�RB�&BWBA�BWBN�BO�BN�BJ�B<wB'�B%�BD�BR�BO�BQ�BB�B5JB6TB@�B9fB;sBK�B<xB3?B4FB3AB-B%�B"�B�B�B}BzB|B�B�B!�B%�B"�B	@BlB]B�zB��B�XB�B�8B�EB��B��B�[B�)Bv�BdcBA�B$�B)B�B�B�SB��BgyBO�BC�B/'BPB
��B
�B
�kB
�2B
�B
��B
�wB
B
Z,B
E�B
(B
�B
�B
!�B
jB	��B	�KB	�bB	�lB	�bB	�XB	�8B	�B	��B	��B	�MB	��B	��B	�_B	�IB	�B	}B	y�B	v�B	u�B	t�B	q�B	l�B	evB	_VB	VB	M�B	K�B	H�B	F�B	E�B	>�B	8lB	9qB	3OB	..B	,#B	+B	)B	& B	�B	�B	�B	�B	
YB	/B�B�B��B��B�B�B�B�B�B�lB�TB�3B�B��B��B��B��B��B��B¯B��B��B�xB�mB�cB�BB�B��B��B��B��B��B��B��B�zB�gB�OB�6B�)B~B{Bw�Bv�Bv�Bt�Bs�Br�Bp�Bo�Bn�Bl�Bk�Bj�Bj�Bj�Bi�Bg�Be�Bd}BcwBaoB^ZB^XB^ZB\OB\MB[IBZ>BZABZBBZABY;BX6BW/BX7BX4BY;BY=BY:BX3B\NB[GB]RB]QB]SB]TB\MB[GBY<B]SB`hB_^BalBalBbrBbsBf�Bh�Bi�Bi�Bk�Bk�Bl�Bk�Bl�Bl�Bp�Bu�Bw�Bx�Bx�B{B|B}BB�HB�dB��B��B��B��B��B��B��B��B��B��B�B�B�6B�gB��B��B��B��BõB��B��B��B��B�B�5B�EB�LB�UB�hB�oB�rB�B�B��B��B��B�B	$B	*B	)B	,B	1B	/B	/B	7B	:B	MB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	'B	'B	'B	'B	(B	)B	(
B	)B	*B	/7B	4UB	5YB	8lB	9qB	:xB	=�B	>�B	?�B	?�B	?�B	A�B	D�B	F�B	H�B	I�B	O�B	RB	UB	Z:B	[?B	\AB	]GB	_WB	h�B	p�B	r�B	x�B	�!B	�!B	� B	�%B	�9B	�DB	�eB	��B	��B	��B	��B	�B	�B	�*B	�4B	�MB	�PB	�VB	�cB	�oB	�uB	�uB	�yB	��B	��B	��B	��B	��B	ìB	��B	��B	��B	ƾB	ƾB	źB	źB	ƾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	� B	�%B	�2B	�AB	�EB	�MB	�JB	�LB	�LB	�LB	�PB	�^B	�cB	�^B	�dB	�dB	�_B	�`B	�iB	�pB	�nB	�uB	�vB	�{B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B

PB
�B
�B
#�B
.*B
5QB
>�B
D�B
I�B
N�B
SB
WB
Z/B
a[B
erB
h�B
l�B
o�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214322016053112143220160531121432  AO  ARCAADJP                                                                    20140721230530    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230530  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230530  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121432  IP                  G�O�G�O�G�O�                
CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:37Z AOML 3.0 creation; 2016-05-31T21:48:57Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230637  20160531144858  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4053_7107_006                   2C  D   APEX                            5370                            041511                          846 @�9��| 1   @�9�����@8�E�����cV�u1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D��D�VfD��fD��fD�	�D�9�D�s3D��fD� D�9�D��3D��fD�fD�C3Dړ3D��fD� D�S3D�vfD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU�=CW��CY��C[��C]��C_�qCa��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D�\Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtl)Dy��D�HD�R�D���D���D�D�6D�o�D���D�{D�6D��D���D��D�?�Dڏ�D���D�{D�O�D�r�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�{A�oA�{A� �A�-A�-A�(�A� �A���A��A��;A��A��#A���A���A���A���A���A���A���A���A��wA��^A���A�A�ƨA�ƨA�t�A���A�$�A���A���A��hA��A�bNA�7LA�-A� �A�&�A�A���A���A�ȴA��FA��+A�jA�^5A�O�A�XA�A�A�"�A�1A�
=A��A���A�v�A�ffA�;dA���A��A���A���A��A�A�A���A�ƨA�JA���A���A�jA�XA�
=A���A���A�5?A�^5A��wA�-A��A�C�A��A�
=A�(�A�
=A�r�A��A���A�/A�VA���A���A��`A��FA��A�JA���A�hsA�5?A��;A��A�C�A�`BA�  A�XA�A�z�A��A���A�G�A�(�A�;dA���A���A��A��A��A��A|�`Ay�mAx^5Aw�At��Arn�Aq7LAn��Al��Aj�/AiAh��AhA�Ae�mAep�Ae7LAd�RAct�AbĜAa��Aa�A`bA]�A]G�A\�A\=qA[�A[O�AZ�jAW�AU��ATI�AR��AQ��AQ�
APv�AN��AN5?AL�uAJ$�AIoAF��AD$�ACp�AB��AA�^A@r�A?�;A?x�A>5?A=�A<�A;l�A:��A9�A8�uA7��A7?}A6��A4�A3��A3��A3;dA2z�A2$�A1ƨA0A�A//A.  A-/A+��A*�`A*��A)��A)l�A(��A($�A'�7A%��A$�`A$bA#&�A"�9A"�uA!��A E�AG�A�DA��A/A�A�AZA��AQ�AJA33A�9AE�A��A�A��A�mA��A$�A?}AjA9XA  Az�A-A{A��AdZA
��A
ȴA
��A
=qA��A��A-AS�AbA�PA�yAZA  A|�A�A5?A�h@�t�@��@�O�@�o@��@��@�/@��@��@���@웦@�33@�?}@��@��@�\)@�/@ޟ�@�j@��@ّh@���@�b@���@�|�@�`B@�z�@ӶF@љ�@Л�@�o@�E�@�V@�5?@�?}@���@˕�@�dZ@��@ɑh@ȴ9@�z�@ȴ9@ȣ�@���@�"�@�-@ļj@�Q�@�(�@Å@�-@��@�A�@�dZ@�ff@��@�V@�1'@�t�@�"�@�^5@�O�@�J@��@��;@��P@��H@�V@��^@��@��y@�5?@���@���@�`B@��7@��@��@���@�C�@���@��9@�b@��F@�;d@���@���@��@�l�@�"�@���@���@�7L@��`@��w@�|�@��@�@�V@�-@��@��T@���@��@���@�p�@�r�@�1@���@�33@��@���@��+@�M�@���@���@���@���@�p�@�/@���@��`@���@��@�9X@��F@�+@��y@�-@���@�@���@���@��-@�x�@��/@�z�@�bN@��u@���@��@�(�@��@�$�@��@���@�G�@�&�@�%@�%@�7L@�O�@�G�@�X@�G�@�G�@�G�@�7L@�V@���@�z�@�r�@�r�@�r�@��@�r�@�ƨ@���@��P@��P@��P@��P@�l�@�;d@�+@�@�~�@�E�@��#@��h@��@��j@��@�(�@�  @��;@��w@���@�S�@�K�@�S�@�dZ@��@���@�C�@�C�@�;d@�o@��@���@���@�@�
=@�
=@�
=@�@���@��@��y@�ȴ@��R@���@�o@�;d@�@�
=@���@���@�M�@�@���@��T@��7@�?}@�X@�hs@��@�x�@�G�@�/@���@��j@���@�r�@�bN@�(�@�Z@�A�@� �@�@�/@y��@r�H@kdZ@e�-@^��@S�
@N�y@G�;@@��@9hs@5�@.��@*=q@%O�@!7L@Z@�9@V@�`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�{A�oA�{A� �A�-A�-A�(�A� �A���A��A��;A��A��#A���A���A���A���A���A���A���A���A��wA��^A���A�A�ƨA�ƨA�t�A���A�$�A���A���A��hA��A�bNA�7LA�-A� �A�&�A�A���A���A�ȴA��FA��+A�jA�^5A�O�A�XA�A�A�"�A�1A�
=A��A���A�v�A�ffA�;dA���A��A���A���A��A�A�A���A�ƨA�JA���A���A�jA�XA�
=A���A���A�5?A�^5A��wA�-A��A�C�A��A�
=A�(�A�
=A�r�A��A���A�/A�VA���A���A��`A��FA��A�JA���A�hsA�5?A��;A��A�C�A�`BA�  A�XA�A�z�A��A���A�G�A�(�A�;dA���A���A��A��A��A��A|�`Ay�mAx^5Aw�At��Arn�Aq7LAn��Al��Aj�/AiAh��AhA�Ae�mAep�Ae7LAd�RAct�AbĜAa��Aa�A`bA]�A]G�A\�A\=qA[�A[O�AZ�jAW�AU��ATI�AR��AQ��AQ�
APv�AN��AN5?AL�uAJ$�AIoAF��AD$�ACp�AB��AA�^A@r�A?�;A?x�A>5?A=�A<�A;l�A:��A9�A8�uA7��A7?}A6��A4�A3��A3��A3;dA2z�A2$�A1ƨA0A�A//A.  A-/A+��A*�`A*��A)��A)l�A(��A($�A'�7A%��A$�`A$bA#&�A"�9A"�uA!��A E�AG�A�DA��A/A�A�AZA��AQ�AJA33A�9AE�A��A�A��A�mA��A$�A?}AjA9XA  Az�A-A{A��AdZA
��A
ȴA
��A
=qA��A��A-AS�AbA�PA�yAZA  A|�A�A5?A�h@�t�@��@�O�@�o@��@��@�/@��@��@���@웦@�33@�?}@��@��@�\)@�/@ޟ�@�j@��@ّh@���@�b@���@�|�@�`B@�z�@ӶF@љ�@Л�@�o@�E�@�V@�5?@�?}@���@˕�@�dZ@��@ɑh@ȴ9@�z�@ȴ9@ȣ�@���@�"�@�-@ļj@�Q�@�(�@Å@�-@��@�A�@�dZ@�ff@��@�V@�1'@�t�@�"�@�^5@�O�@�J@��@��;@��P@��H@�V@��^@��@��y@�5?@���@���@�`B@��7@��@��@���@�C�@���@��9@�b@��F@�;d@���@���@��@�l�@�"�@���@���@�7L@��`@��w@�|�@��@�@�V@�-@��@��T@���@��@���@�p�@�r�@�1@���@�33@��@���@��+@�M�@���@���@���@���@�p�@�/@���@��`@���@��@�9X@��F@�+@��y@�-@���@�@���@���@��-@�x�@��/@�z�@�bN@��u@���@��@�(�@��@�$�@��@���@�G�@�&�@�%@�%@�7L@�O�@�G�@�X@�G�@�G�@�G�@�7L@�V@���@�z�@�r�@�r�@�r�@��@�r�@�ƨ@���@��P@��P@��P@��P@�l�@�;d@�+@�@�~�@�E�@��#@��h@��@��j@��@�(�@�  @��;@��w@���@�S�@�K�@�S�@�dZ@��@���@�C�@�C�@�;d@�o@��@���@���@�@�
=@�
=@�
=@�@���@��@��y@�ȴ@��R@���@�o@�;d@�@�
=@���@���@�M�@�@���@��T@��7@�?}@�X@�hs@��@�x�@�G�@�/@���@��j@���@�r�@�bN@�(�@�Z@�A�@� �@�@�/@y��@r�H@kdZ@e�-@^��@S�
@N�y@G�;@@��@9hs@5�@.��@*=q@%O�@!7L@Z@�9@V@�`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B �B%�B,B7LBC�BF�BP�BQ�BS�BT�BT�BVBVBW
BW
BW
BVBW
BW
BW
BW
BVBVBVBT�BZB�%B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�9B�3B�3B�!B�B�B�!B�B�B��B��B��B�VB�7B{�Bu�Bv�Br�Bn�B]/BO�BE�BaHBXBZB�BƨB�3B��B�DBgmBI�B6FB%�B�B1B��B�fBÖB�RB�FB�-B�B��B��B�\Bo�BP�B7LB+B!�B�B{BVBB
�B
�BB
�B
B
��B
�VB
n�B
L�B
33B
�B
{B
DB	��B	�B	�mB	�B	��B	B	�^B	�9B	�B	��B	��B	��B	��B	�oB	�VB	�DB	�%B	}�B	q�B	m�B	iyB	ffB	cTB	]/B	VB	G�B	?}B	8RB	2-B	0!B	.B	(�B	"�B	�B	�B	hB	JB	B��B��B��B�B�B�B�B�B�B�yB�sB�fB�NB�BB�BB�;B�)B�
B�B��B��B��B��B��BȴBĜB��B�jB�XB�FB�?B�3B�'B�B�B��B��B��B��B��B��B��B�hB�PB�=B�+B�B�B~�Bz�Bu�Bp�Bo�Bm�BjBhsBffBcTB_;B[#BXBT�BQ�BP�BO�BN�BM�BN�BO�BQ�BW
B[#B]/B]/B\)B[#BW
BS�BQ�BP�BM�BL�BK�BJ�BI�BF�BC�B?}B:^B2-B+B&�B&�B$�B"�B"�B!�B!�B!�B �B�B�B�B�B�B�BhB\BVBVBVBVB\BVBJBJBDB\BVBhB�B�B �B!�B#�B"�B"�B#�B'�B(�B0!B;dB>wBA�BG�BG�BI�BI�BI�BJ�BL�BO�BQ�BS�BVBVBW
BW
BW
BVBT�BS�BZBZB]/BaHBcTBe`Be`BdZBaHBbNBgmBffBk�Bm�Bn�Bn�Bl�BhsBl�Bp�Bo�Bo�Bq�Br�Bs�Bw�By�Bz�B|�B�+B�DB�PB�oB��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�FB�XB�qB�}B��BŢBɺB��B��B��B��B�
B�
B�B�B�;B�HB�ZB�fB�B�B��B��B��B��B��B��B	B	B	%B	JB	\B	hB	uB	{B	�B	�B	�B	�B	"�B	$�B	/B	33B	6FB	8RB	:^B	=qB	@�B	B�B	D�B	I�B	L�B	N�B	O�B	P�B	R�B	XB	_;B	aHB	bNB	cTB	cTB	cTB	e`B	hsB	jB	k�B	n�B	o�B	r�B	s�B	v�B	y�B	z�B	}�B	~�B	�B	�B	�B	�B	�%B	�+B	�7B	�JB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�-B	�-B	�-B	�-B	�3B	�3B	�?B	�RB	�dB	��B	��B	B	ĜB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	�#B	�B	��B

=B
VB
�B
"�B
,B
33B
;dB
C�B
J�B
N�B
Q�B
W
B
]/B
`BB
e`B
hsB
jB
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B �B%�B,B7ZBC�BF�BP�BQ�BT
BUBUBVBVBWBWBWBVBWBWBWBWBVBVBVBUBZ(B�6B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�	B�,B�B�B�-B�>B�IB�DB�CB�1B�B�&B�2B�'B�B��B��B��B�bB�IB{�Bu�Bv�Br�Bn�B]>BO�BE�BaWBXBZ-B�BƲB�?B��B�NBg{BI�B6NB%�B�B;B��B�pBÞB�]B�QB�7B�B��B��B�hBo�BP�B7TB+B!�B�B�BbBB
�B
�QB
�B
B
�B
�dB
n�B
L�B
3GB
�B
�B
YB	��B	�B	�B	�%B	��B	¤B	�uB	�NB	�'B	��B	��B	��B	��B	��B	�nB	�ZB	�;B	~B	q�B	m�B	i�B	f}B	cmB	]HB	VB	G�B	?�B	8lB	2GB	0>B	.0B	)B	"�B	�B	�B	�B	gB	$B��B��B��B��B�B��B�B�B�B�B�B�B�mB�aB�aB�ZB�IB�(B�"B�B�B�B��B��B��BĽB��B��B�yB�gB�_B�QB�GB�;B�(B�B��B��B��B��B��B��B��B�rB�`B�OB�4B�(BB{Bu�Bp�Bo�Bm�Bj�Bh�Bf�BcyB_aB[IBX6BU!BRBQBPBO BM�BO BPBRBW-B[HB]SB]TB\MB[IBW2BTBRBQBM�BL�BK�BJ�BI�BF�BC�B?�B:�B2UB+&B'B'B%B"�B"�B!�B!�B!�B �B�B�B�B�B�B�BvB�BaBfBdBcB�BeBYBrBSB�BdBuB�B�B �B!�B#�B"�B"�B#�B(B)B0GB;�B>�BA�BG�BG�BI�BI�BI�BJ�BL�BPBRBTBV*BV+BW/BW.BW/BV(BU!BTBZ@BZBB]RBalBczBe�Be�Bd{BamBbrBg�Bf�Bk�Bm�Bn�Bn�Bl�Bh�Bl�Bp�Bo�Bo�Bq�Br�Bs�Bw�By�B{B}B�NB�hB�rB��B��B��B��B��B��B��B�B�B�'B�/B�7B�=B�6B�CB�fB�vB��B��B��B��B��B��B��B��B�B�*B�+B�-B�5B�[B�hB�yB�B�B��B��B��B�B�B�B�B	/B	7B	DB	gB	xB	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	/6B	3PB	6aB	8mB	:zB	=�B	@�B	B�B	D�B	I�B	L�B	N�B	O�B	P�B	SB	X'B	_UB	abB	bgB	cnB	cnB	cnB	ezB	h�B	j�B	k�B	n�B	o�B	r�B	s�B	v�B	y�B	z�B	~B	B	�!B	�+B	�1B	�7B	�=B	�DB	�OB	�cB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�-B	�8B	�7B	�CB	�FB	�DB	�EB	�MB	�KB	�UB	�iB	�}B	��B	��B	¦B	ĳB	��B	ƾB	ƿB	��B	��B	��B	��B	��B	��B	��B	�B	�:B	�B	�B

SB
kB
�B
"�B
,B
3HB
;xB
C�B
J�B
N�B
Q�B
WB
]AB
`TB
erB
h�B
j�B
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448582016053114485820160531144858  AO  ARCAADJP                                                                    20140721230637    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230637  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230637  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144858  IP                  G�O�G�O�G�O�                
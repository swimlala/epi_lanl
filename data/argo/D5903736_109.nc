CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-12T19:16:22Z AOML 3.0 creation; 2016-05-31T19:14:42Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150412191622  20160531121442  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               mA   AO  4051_7090_109                   2C  D   APEX                            5368                            041511                          846 @�H��
`1   @�H�c]��@3�C��%�d`��E�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    mA   A   A   @�  @���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D�fD�0 D�l�D��fD��D�C3D��3D���D� D�6fD�|�D��fD�	�D�@ Dڌ�D��D� D�)�D�ffD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @x��@�G�@�G�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��D\D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1\D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtr�Dy��D��D�,{D�iHD���D��D�?�D���D��D�{D�2�D�yHD���D�D�<{DډHD�HD�{D�&D�b�D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A̧�A̲-A̲-A̲-A̲-A̩�Ạ�A̬Ạ�A̝�A̕�A̅A�t�A�n�A�%A���A�G�AɓuA�\)A���Aƙ�A��A�r�A�
=Aĩ�A�ȴA�\)A�A�`BA�C�A�{A���A�VA�?}A���A�S�A��yA�1A�A�ƨA��A�G�A��A�;dA�C�A�ffA�1'A�bA�~�A�S�A�VA��TA�`BA�&�A��`A�9XA��TA���A��!A�jA�|�A�jA�=qA��+A��hA���A�A��^A���A��#A�{A�ffA�JA��A�I�A��A��/A�ZA��uA�$�A��A��9A��A���A���A�A�K�A�|�A��TA���A�\)A�JA�9XA�&�A���A�{A�?}A�A�A�|�A~ZA|�AyAw�Au��Ar��Aq
=Ap$�Ao
=Am��Aj��Af�HAe�hAc7LA`�jA_
=A]��A\(�AX�HAV�AU��AR�yAQ��AQG�AP-AM`BAJ�9AG��AF��AE�AE?}AD�/AD$�ACS�ABn�AA��A@  A>9XA<�A<  A;�-A;l�A;oA:I�A9�A6��A5�A3��A1�mA1hsA0  A.bNA-�#A-�A+�TA*�A*ZA)��A&��A&E�A%`BA$�/A$  A"ȴA"bNA"E�A"JA!�A��A�9A�AS�A�wA33A�A��Ar�AjA
=A/A�!A�A��A��A��A�#A�-A�yA1'A;dA�A�wA�\AƨA�#A V@��@�K�@�Ĝ@�A�@���@�O�@��
@��T@��D@���@��@���@���@땁@���@��@� �@�7@�C�@���@���@�|�@�ff@�j@�o@�E�@�bN@�S�@��/@�r�@�1@�S�@ҧ�@ҟ�@�&�@�|�@�@�p�@��@̋D@��@�C�@�
=@�~�@��#@�hs@�7L@�bN@�9X@�9X@���@�;d@Ɨ�@��@���@�7L@ēu@��@�33@���@�~�@��^@�x�@���@��@�K�@���@�M�@�ff@�M�@�@��7@���@�I�@��P@���@�ff@�V@�=q@��@���@��T@��#@���@�x�@�x�@�%@�r�@�1@�|�@�V@�@��T@���@���@��@�Q�@��m@��w@�
=@��@���@��h@�x�@�`B@�7L@��/@�j@�1@���@�dZ@�+@��@���@��+@�E�@��7@���@�I�@���@��@�+@�ȴ@��+@�^5@�^5@�V@�5?@��@��@��@�G�@�7L@��`@�A�@���@�|�@�33@��\@�$�@��@�@��@�G�@��`@�Q�@�  @�ƨ@�ƨ@���@���@���@���@��@���@�=q@�{@�@��^@���@��@�r�@��;@��@���@��P@���@�\)@��@�v�@�@�{@�$�@���@�G�@�%@��/@��D@�Q�@�(�@�t�@�S�@�;d@�o@���@��!@�^5@��@�x�@�?}@�&�@��@�%@��`@�Ĝ@���@� �@��
@���@�l�@�+@���@�^5@�=q@��@���@�X@���@���@��`@���@�Ĝ@�r�@�Z@�I�@�9X@�1'@� �@��@��F@��@�dZ@�C�@�
=@�ȴ@���@���@�V@��@�@���@�x�@�O�@�/@�&�@�%@��j@��@�Q�@�1@��P@�C�@�33@�"�@��@�n�@��@�@���@���@���@�j@�ƨ@�|�@��@��@��@�ȴ@���@�M�@���@�p�@�?}@��@�Z@�Q�@�I�@�1'@��@���@�S�@�33@�+@�
=@��R@�V@���@��#@��@�X@�O�@�G�@�?}@�Ĝ@��@�I�@��;@�
=@�
=@�
=@�o@��y@�~�@��9@w+@q7L@i�^@b�\@Zn�@RM�@JJ@A�^@:-@3"�@-�@'��@!��@�m@��@t�@�@��@	�7@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A̧�A̲-A̲-A̲-A̲-A̩�Ạ�A̬Ạ�A̝�A̕�A̅A�t�A�n�A�%A���A�G�AɓuA�\)A���Aƙ�A��A�r�A�
=Aĩ�A�ȴA�\)A�A�`BA�C�A�{A���A�VA�?}A���A�S�A��yA�1A�A�ƨA��A�G�A��A�;dA�C�A�ffA�1'A�bA�~�A�S�A�VA��TA�`BA�&�A��`A�9XA��TA���A��!A�jA�|�A�jA�=qA��+A��hA���A�A��^A���A��#A�{A�ffA�JA��A�I�A��A��/A�ZA��uA�$�A��A��9A��A���A���A�A�K�A�|�A��TA���A�\)A�JA�9XA�&�A���A�{A�?}A�A�A�|�A~ZA|�AyAw�Au��Ar��Aq
=Ap$�Ao
=Am��Aj��Af�HAe�hAc7LA`�jA_
=A]��A\(�AX�HAV�AU��AR�yAQ��AQG�AP-AM`BAJ�9AG��AF��AE�AE?}AD�/AD$�ACS�ABn�AA��A@  A>9XA<�A<  A;�-A;l�A;oA:I�A9�A6��A5�A3��A1�mA1hsA0  A.bNA-�#A-�A+�TA*�A*ZA)��A&��A&E�A%`BA$�/A$  A"ȴA"bNA"E�A"JA!�A��A�9A�AS�A�wA33A�A��Ar�AjA
=A/A�!A�A��A��A��A�#A�-A�yA1'A;dA�A�wA�\AƨA�#A V@��@�K�@�Ĝ@�A�@���@�O�@��
@��T@��D@���@��@���@���@땁@���@��@� �@�7@�C�@���@���@�|�@�ff@�j@�o@�E�@�bN@�S�@��/@�r�@�1@�S�@ҧ�@ҟ�@�&�@�|�@�@�p�@��@̋D@��@�C�@�
=@�~�@��#@�hs@�7L@�bN@�9X@�9X@���@�;d@Ɨ�@��@���@�7L@ēu@��@�33@���@�~�@��^@�x�@���@��@�K�@���@�M�@�ff@�M�@�@��7@���@�I�@��P@���@�ff@�V@�=q@��@���@��T@��#@���@�x�@�x�@�%@�r�@�1@�|�@�V@�@��T@���@���@��@�Q�@��m@��w@�
=@��@���@��h@�x�@�`B@�7L@��/@�j@�1@���@�dZ@�+@��@���@��+@�E�@��7@���@�I�@���@��@�+@�ȴ@��+@�^5@�^5@�V@�5?@��@��@��@�G�@�7L@��`@�A�@���@�|�@�33@��\@�$�@��@�@��@�G�@��`@�Q�@�  @�ƨ@�ƨ@���@���@���@���@��@���@�=q@�{@�@��^@���@��@�r�@��;@��@���@��P@���@�\)@��@�v�@�@�{@�$�@���@�G�@�%@��/@��D@�Q�@�(�@�t�@�S�@�;d@�o@���@��!@�^5@��@�x�@�?}@�&�@��@�%@��`@�Ĝ@���@� �@��
@���@�l�@�+@���@�^5@�=q@��@���@�X@���@���@��`@���@�Ĝ@�r�@�Z@�I�@�9X@�1'@� �@��@��F@��@�dZ@�C�@�
=@�ȴ@���@���@�V@��@�@���@�x�@�O�@�/@�&�@�%@��j@��@�Q�@�1@��P@�C�@�33@�"�@��@�n�@��@�@���@���@���@�j@�ƨ@�|�@��@��@��@�ȴ@���@�M�@���@�p�@�?}@��@�Z@�Q�@�I�@�1'@��@���@�S�@�33@�+@�
=@��R@�V@���@��#@��@�X@�O�@�G�@�?}@�Ĝ@��@�I�@��;@�
=@�
=@�
=@�o@��y@�~�@��9@w+@q7L@i�^@b�\@Zn�@RM�@JJ@A�^@:-@3"�@-�@'��@!��@�m@��@t�@�@��@	�7@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB>wB=qB=qB<jB>wBA�B>wB?}BA�BC�BF�BH�BJ�Bk�B�^B��B�BhB)�B,B/B7LB@�BB�BP�BdZB]/BM�BB�BYB��B�NB��BJB�B%�B�B\B��B�yB�NB�ZB�`B�B�dB�?B�jB�qB�FB��B��B��B��B��B��B��B��B�oB�7By�BjB[#BR�BC�B-B#�BhB�B��B�LB�B��B��B�hB�PB�7B�Bs�BgmBl�B`BBT�BL�B9XB�B
��B
�ZB
��B
�FB
��B
�B
t�B
bNB
XB
O�B
D�B
6FB
 �B
hB
+B	��B	�ZB	��B	B	�FB	�!B	��B	��B	�DB	u�B	k�B	_;B	Q�B	I�B	B�B	9XB	,B	%�B	 �B	�B	�B	�B	{B	DB��B�B�B�yB�fB�ZB�HB�/B�B�B��B��BȴB��B��B��B��B��BǮB�wB�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�\B�PB�JB�DB�7B�B�B}�B{�By�Bw�Bv�Bt�Br�Bo�Bm�BgmBbNB`BB_;B_;BdZBe`BgmBffBcTBbNBe`BgmBiyBiyBhsBe`Be`BgmBiyBl�Bk�Bn�Bs�Bt�Bt�Br�Bo�Bn�Bk�BjBk�Bp�Bs�Bt�Bt�Bu�Bu�Bv�Bv�Bx�B{�B~�B�1B�=B�VB�\B�bB�hB�oB��B��B��B��B��B��B��B�B�B�3B�FB�^B�jB�dB�dB�dB�jB�}BÖBǮB��B��B��B��B�#B�HB�mB��B��B��B��B��B	  B	B	B	+B	+B	1B	1B		7B	
=B	DB	PB	PB	VB	bB	uB	{B	�B	�B	�B	�B	�B	�B	 �B	$�B	&�B	'�B	-B	5?B	9XB	9XB	:^B	<jB	?}B	C�B	F�B	H�B	J�B	K�B	L�B	N�B	P�B	P�B	P�B	S�B	YB	[#B	]/B	_;B	aHB	cTB	e`B	ffB	gmB	jB	m�B	o�B	q�B	t�B	u�B	u�B	v�B	v�B	y�B	y�B	{�B	�B	�B	�B	�B	�1B	�7B	�DB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�?B	�FB	�RB	�^B	�jB	�jB	��B	B	ÖB	ÖB	ĜB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�/B	�;B	�;B	�;B	�HB	�HB	�TB	�TB	�TB	�TB	�TB	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B

=B

=B

=B
DB
JB
PB
PB
PB
JB
VB
bB
hB
oB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
&�B
-B
1'B
9XB
>wB
D�B
J�B
Q�B
XB
]/B
aHB
ffB
l�B
q�B
s�B
w�B
z�B
}�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B>�B>�B=|B=yB<vB>�BA�B>�B?�BA�BC�BF�BH�BJ�Bk�B�iB��B�BsB*B,B/&B7XB@�BB�BP�BdfB];BM�BB�BYB��B�[B��BXB�B%�B�BjB��B�B�\B�cB�pB�)B�mB�GB�yB�~B�OB��B��B��B��B��B��B��B��B�zB�?By�Bj�B[,BR�BC�B-B#�BsB�B��B�VB�B��B��B�sB�YB�CB�Bs�BgyBl�B`LBU	BL�B9bB�B
��B
�eB
�B
�TB
��B
�B
t�B
b_B
X"B
O�B
D�B
6WB
 �B
yB
=B	��B	�lB	�B	§B	�ZB	�6B	�B	��B	�ZB	u�B	k�B	_UB	RB	I�B	B�B	9pB	,!B	%�B	 �B	�B	�B	�B	�B	aB�B��B�B�B�B�yB�gB�OB�=B�$B�B��B��B��B��B��B��B��B��B��B�ZB�6B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�sB�lB�dB�WB�AB�1B~B|BzBw�Bv�Bt�Br�Bo�Bm�Bg�BbrB`gB_`B_^Bd|Be�Bg�Bf�BcyBbsBe�Bg�Bi�Bi�Bh�Be�Be�Bg�Bi�Bl�Bk�Bn�Bs�Bt�Bt�Br�Bo�Bn�Bk�Bj�Bk�Bp�Bs�Bt�Bt�Bu�Bu�Bv�Bv�Bx�B|
B B�SB�_B�uB��B��B��B��B��B��B��B��B��B��B��B�$B�4B�TB�fB�~B��B��B��B��B��B��BöB��B��B��B��B�B�AB�gB�B��B��B��B��B�B	 B	2B	<B	KB	GB	OB	NB		SB	
WB	aB	pB	mB	rB	}B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	'B	(B	-+B	5ZB	9qB	9qB	:yB	<�B	?�B	C�B	F�B	H�B	J�B	K�B	L�B	N�B	QB	QB	Q B	TB	Y4B	[;B	]HB	_TB	aaB	coB	ezB	f�B	g�B	j�B	m�B	o�B	q�B	t�B	u�B	u�B	v�B	v�B	y�B	y�B	| B	�!B	�+B	�3B	�7B	�HB	�PB	�^B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�+B	�0B	�5B	�8B	�@B	�BB	�DB	�OB	�WB	�\B	�jB	�vB	��B	��B	��B	¨B	îB	íB	ĳB	İB	ƿB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�$B	�2B	�GB	�OB	�OB	�QB	�^B	�]B	�kB	�jB	�jB	�iB	�kB	�vB	�vB	�vB	�vB	�vB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�	B
 B
B
!B
!B
(B
&B
'B
/B
:B
?B
HB
	KB
	KB

QB

QB

UB
YB
^B
cB
eB
eB
`B
kB
yB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
&�B
-"B
19B
9kB
>�B
D�B
J�B
Q�B
X#B
]BB
aYB
fyB
l�B
q�B
s�B
w�B
z�B
~B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214422016053112144220160531121442  AO  ARCAADJP                                                                    20150412191622    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150412191622  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150412191622  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121442  IP                  G�O�G�O�G�O�                
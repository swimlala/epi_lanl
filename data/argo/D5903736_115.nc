CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-13T09:15:51Z AOML 3.0 creation; 2016-05-31T19:14:43Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150613091551  20160531121443  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               sA   AO  4051_7090_115                   2C  D   APEX                            5368                            041511                          846 @�W�ww?�1   @�W���?�@4�z�G��dC�vȴ91   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    sA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�DyY�D��D�P D��fD�ٚD�3D�C3D��fD���D�3D�S3D���D�� D�fD�33Dڃ3Dਗ਼D�fD�@ D�fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��G@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'��B/��B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm�qCo��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dl\Dl�\Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dte�DyR�D�	HD�L{D���D��D���D�?�D���D��D���D�O�D��DǼ{D��D�/�D��D�D��D�<{D��D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`AծA�^5A�/A�A��A��#A���A���A���A�ȴA�Aԝ�A���AѲ-Aѡ�Aѝ�Aя\A�(�A�bA���A�M�A�z�Aδ9A�bA��`A��AˋDA��A�^5A�{A���A�E�A�ȴAƍPA�~�A�$�AÝ�A���A�Aº^A§�A�XA�oA���A��A���A���A���A�ȴA�dZA��A�dZA�9XA���A�l�A�=qA���A�$�A��hA�G�A�-A���A�VA�|�A��A��DA��A��#A��A��A��A�hsA�{A�(�A��-A��A�bNA���A��uA�ȴA�K�A��A��uA��yA��
A��FA���A��hA�?}A���A�v�A���A�oA�  A���A�Q�A��wA���A��9A�JA��A�"�A�hsA��A�bA��RA���A��A��A��A��A�~�A�A�A�G�A�K�A�JA��^A���A�ȴA�p�A�\)A�E�A�K�A���A�Q�A��HA}/A{G�AyƨAv�\Ar�Ap��Am�#Ai\)Ag��Ae��Ad��Ac��Aax�A_t�A^~�A\�AZ��AW�mAS�FAQ+AOp�AN��AM��AJ�/AH{AF�`AF�9AE�mAD�`AD5?AC%A?33A=oA<~�A<9XA;��A;%A:(�A8�A7�A7p�A6�/A5t�A4  A2�A1��A0�jA/�mA/33A,��A* �A)|�A)/A(ĜA(I�A'�A'��A%hsA"�A!A ��A   A\)AK�A��A��AȴAr�A�A��AffA��Ax�A;dAXA&�A�AjA��AdZAZA�;A��A�/AVAA �A�DAt�AffAA�A��A
E�A~�A��A�yA��A1@��y@�hs@��D@� �@��\@�ƨ@�r�@�=q@�bN@�  @�X@�@� �@��y@��@��@ߍP@ޟ�@��@�t�@�M�@�&�@׾w@���@�^5@��#@�V@�  @�t�@�M�@���@�Z@��@�=q@ͩ�@́@���@̣�@̛�@�r�@�Q�@�1'@��
@˝�@��@ʰ!@�V@�E�@�@�7L@��
@�K�@�S�@�S�@�K�@�"�@�ȴ@�M�@���@Ų-@�p�@���@� �@þw@�~�@���@�X@���@��@�b@�;d@�@�`B@�O�@�?}@�/@��/@�Q�@�b@���@��
@�\)@���@�n�@��@��-@��@�b@�ƨ@��@���@�@�ȴ@�n�@���@�p�@�&�@��j@� �@�|�@��@��@���@��@�X@��@�r�@��F@�dZ@�;d@�+@��y@�-@���@�p�@�X@�O�@�G�@���@��/@�Ĝ@��9@��@���@�I�@��
@�|�@��@��@���@�n�@�M�@�M�@�E�@�5?@�$�@�{@�{@�{@�J@���@��@���@�`B@�?}@��@���@���@�A�@�ƨ@�C�@��H@��\@�v�@�ff@�$�@��@��#@��h@�`B@�%@���@��u@�r�@�Z@�Q�@�I�@��@�+@�
=@��H@���@���@�ff@�-@�$�@�J@���@��^@��@���@�A�@���@��@�S�@��@���@�^5@�5?@��#@�hs@��@��@���@��D@�bN@�bN@�bN@�bN@�I�@�Z@�Z@���@�33@�
=@�@��@��@�ȴ@��R@�~�@�5?@��-@��h@��7@��@�x�@�O�@�V@��9@�I�@�b@��w@�;d@��H@���@�M�@�`B@���@��@�Z@�A�@�1'@�1@���@�@�~�@�J@���@��7@�X@��@�Ĝ@�I�@���@�"�@�
=@�o@��R@�n�@�5?@��@���@�%@�r�@��@��
@��F@�l�@�|�@�dZ@��@���@���@�l�@|�@t1@i�@ax�@["�@S"�@L�@Co@97L@2�@+@'�@"��@
=@-@��@�@�m@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��`AծA�^5A�/A�A��A��#A���A���A���A�ȴA�Aԝ�A���AѲ-Aѡ�Aѝ�Aя\A�(�A�bA���A�M�A�z�Aδ9A�bA��`A��AˋDA��A�^5A�{A���A�E�A�ȴAƍPA�~�A�$�AÝ�A���A�Aº^A§�A�XA�oA���A��A���A���A���A�ȴA�dZA��A�dZA�9XA���A�l�A�=qA���A�$�A��hA�G�A�-A���A�VA�|�A��A��DA��A��#A��A��A��A�hsA�{A�(�A��-A��A�bNA���A��uA�ȴA�K�A��A��uA��yA��
A��FA���A��hA�?}A���A�v�A���A�oA�  A���A�Q�A��wA���A��9A�JA��A�"�A�hsA��A�bA��RA���A��A��A��A��A�~�A�A�A�G�A�K�A�JA��^A���A�ȴA�p�A�\)A�E�A�K�A���A�Q�A��HA}/A{G�AyƨAv�\Ar�Ap��Am�#Ai\)Ag��Ae��Ad��Ac��Aax�A_t�A^~�A\�AZ��AW�mAS�FAQ+AOp�AN��AM��AJ�/AH{AF�`AF�9AE�mAD�`AD5?AC%A?33A=oA<~�A<9XA;��A;%A:(�A8�A7�A7p�A6�/A5t�A4  A2�A1��A0�jA/�mA/33A,��A* �A)|�A)/A(ĜA(I�A'�A'��A%hsA"�A!A ��A   A\)AK�A��A��AȴAr�A�A��AffA��Ax�A;dAXA&�A�AjA��AdZAZA�;A��A�/AVAA �A�DAt�AffAA�A��A
E�A~�A��A�yA��A1@��y@�hs@��D@� �@��\@�ƨ@�r�@�=q@�bN@�  @�X@�@� �@��y@��@��@ߍP@ޟ�@��@�t�@�M�@�&�@׾w@���@�^5@��#@�V@�  @�t�@�M�@���@�Z@��@�=q@ͩ�@́@���@̣�@̛�@�r�@�Q�@�1'@��
@˝�@��@ʰ!@�V@�E�@�@�7L@��
@�K�@�S�@�S�@�K�@�"�@�ȴ@�M�@���@Ų-@�p�@���@� �@þw@�~�@���@�X@���@��@�b@�;d@�@�`B@�O�@�?}@�/@��/@�Q�@�b@���@��
@�\)@���@�n�@��@��-@��@�b@�ƨ@��@���@�@�ȴ@�n�@���@�p�@�&�@��j@� �@�|�@��@��@���@��@�X@��@�r�@��F@�dZ@�;d@�+@��y@�-@���@�p�@�X@�O�@�G�@���@��/@�Ĝ@��9@��@���@�I�@��
@�|�@��@��@���@�n�@�M�@�M�@�E�@�5?@�$�@�{@�{@�{@�J@���@��@���@�`B@�?}@��@���@���@�A�@�ƨ@�C�@��H@��\@�v�@�ff@�$�@��@��#@��h@�`B@�%@���@��u@�r�@�Z@�Q�@�I�@��@�+@�
=@��H@���@���@�ff@�-@�$�@�J@���@��^@��@���@�A�@���@��@�S�@��@���@�^5@�5?@��#@�hs@��@��@���@��D@�bN@�bN@�bN@�bN@�I�@�Z@�Z@���@�33@�
=@�@��@��@�ȴ@��R@�~�@�5?@��-@��h@��7@��@�x�@�O�@�V@��9@�I�@�b@��w@�;d@��H@���@�M�@�`B@���@��@�Z@�A�@�1'@�1@���@�@�~�@�J@���@��7@�X@��@�Ĝ@�I�@���@�"�@�
=@�o@��R@�n�@�5?@��@���@�%@�r�@��@��
@��F@�l�@�|�@�dZ@��@���@���@�l�@|�@t1@i�@ax�@["�@S"�@L�@Co@97L@2�@+@'�@"��@
=@-@��@�@�m@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�%B
�7B
�VB
�\B
�\B
�VB
�VB
�VB
�VB
�PB
�PB
�PB
�7B
z�B
x�B
�B
�7B
�\B
�-B
�B
�B33BcTBŢB��B%�B/B;dB�B�?B�LB�jB�RB�?BȴB��BÖB�}B�}B�NB��B+B�B)�B.B0!B8RB:^B8RB;dB7LB6FB49B49B1'B;dBC�BG�BI�BL�BbNBt�Bu�Bl�B_;BVBM�BK�B8RB33B33B1'B0!B%�B�BPB%B��B��B�yB�TB�yB�B�mB�;B�BB�B(�B)�B,B"�B�B2-B2-B,BPB��B�BB�?B{�BjBP�B=qB(�B��B�BB��B�B�;B	7B+B�B�
B�B�VB�%B{�BdZBD�B,B�B	7B
��B
�)B
��B
��B
VB
=qB
-B
hB	��B	�fB	��B	�-B	��B	��B	�{B	�DB	|�B	r�B	jB	e`B	YB	F�B	:^B	,B	�B	�B	�B	�B	1B	B	B��B��B��B�B�NB�#B�B�B��B��B��B��B��BɺBǮBŢBB��B�wB�qB�dB�RB�LB�XB�XB�RB�RB�RB�LB�9B�'B�B��B�B�B�B�'B�'B�?BB��B�FB�'B�'B�-B�FB�wB��B��BŢB��B��B��B��B��B��B��B��B��BƨB��B�^B�-B�!B�B��B��B��B�bB�VB�oB�PB�=B�1B�%B�B}�B{�Bz�Bx�By�Bz�Bz�By�Bz�B{�Bz�B{�B|�B|�B{�B}�B~�B�B�B�%B�+B�7B�PB�VB�hB��B��B��B��B��B��B��B�B�-B�-B�-B�-B�?B�RB�dB�qB�wB�}B�}B��BÖBĜBĜBÖBÖBĜBŢBƨBȴBɺBɺB��B��B��B��B�B�B�B�#B�/B�BB�sB�B�B�B�B�B��B��B��B��B��B	B	B	%B	1B	DB	bB	�B	�B	�B	#�B	'�B	+B	0!B	33B	5?B	7LB	;dB	@�B	C�B	C�B	D�B	F�B	I�B	J�B	M�B	Q�B	S�B	T�B	T�B	VB	ZB	]/B	_;B	_;B	_;B	`BB	aHB	bNB	cTB	cTB	cTB	cTB	e`B	hsB	jB	l�B	m�B	o�B	p�B	p�B	p�B	q�B	q�B	q�B	r�B	r�B	r�B	r�B	r�B	r�B	t�B	u�B	u�B	u�B	v�B	w�B	z�B	}�B	� B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�7B	�DB	�JB	�PB	�VB	�\B	�\B	�\B	�hB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�3B	�9B	�FB	�XB	�qB	ÖB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�HB	�NB	�ZB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
+B
bB
�B
�B
#�B
$�B
(�B
2-B
8RB
A�B
K�B
P�B
W
B
[#B
^5B
dZB
jB
q�B
t�B
x�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�5B
�EB
�`B
�jB
�gB
�aB
�eB
�`B
�bB
�^B
�\B
�_B
�EB
z�B
x�B
�B
�CB
�iB
�:B
�B
�B3>Bc]BŬB��B%�B/$B;mB�B�MB�WB�sB�^B�LB��B��BàB��B��B�[B�B4B�B*	B.!B01B8^B:lB8bB;pB7YB6XB4GB4FB15B;pBC�BG�BI�BL�Bb]Bt�Bu�Bl�B_JBVBM�BK�B8[B3?B3@B16B0/B%�B�BYB2B��B��B�B�_B�B�B�wB�HB��BB�B)B*B,B"�B�B2:B2<B,B\B��B�QB�KB{�Bj�BP�B=|B) B��B�KB��B�B�FB	>B4B�B�B�B�dB�.B{�BdcBD�B,B�B	EB
��B
�8B
��B
��B
VB
=�B
- B
zB	��B	�xB	��B	�CB	�B	��B	��B	�ZB	}B	r�B	j�B	exB	Y0B	F�B	:xB	,"B	�B	�B	�B	�B	MB	-B	#B�B��B��B�B�nB�@B�6B�.B�B�
B��B��B��B��B��BſB±B��B��B��B��B�pB�mB�wB�wB�rB�rB�rB�lB�YB�IB�(B�B�#B�6B�9B�FB�GB�`B®B��B�fB�HB�GB�LB�eB��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�~B�OB�AB�*B��B��B��B��B�xB��B�rB�aB�TB�IB�.B~B|
B{Bx�By�B{B{By�B{B|
B{B|
B}B}B|
B~BB�0B�<B�HB�PB�ZB�tB�yB��B��B��B��B��B��B�B�
B�(B�KB�LB�MB�NB�_B�qB��B��B��B��B��B��B÷BľBļBõBöBĽB��B��B��B��B��B��B��B��B�B�/B�6B�;B�@B�MB�aB�B�B�B�B�B�B��B��B��B� B�B	1B	8B	BB	MB	aB	B	�B	�B	�B	#�B	(	B	+B	0<B	3PB	5[B	7fB	;B	@�B	C�B	C�B	D�B	F�B	I�B	J�B	M�B	RB	TB	UB	UB	VB	Z:B	]JB	_UB	_VB	_VB	`^B	acB	bfB	cnB	cmB	cmB	cnB	eyB	h�B	j�B	l�B	m�B	o�B	p�B	p�B	p�B	q�B	q�B	q�B	r�B	r�B	r�B	r�B	r�B	r�B	t�B	u�B	u�B	u�B	v�B	w�B	z�B	~B	�B	�"B	�+B	�+B	�1B	�:B	�<B	�<B	�AB	�NB	�\B	�bB	�hB	�oB	�rB	�tB	�rB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�,B	�8B	�NB	�LB	�OB	�^B	�oB	��B	íB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�=B	�>B	�FB	�DB	�DB	�NB	�QB	�_B	�bB	�pB	�|B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�	B	�B	�B	�	B	�B	��B	��B	�
B	�B	�
B	�	B	�B	�B
 B
B
B
(B
BB
vB
�B
�B
#�B
$�B
)	B
2>B
8eB
A�B
K�B
P�B
WB
[7B
^EB
dkB
j�B
q�B
t�B
x�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214432016053112144320160531121443  AO  ARCAADJP                                                                    20150613091551    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150613091551  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150613091551  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121443  IP                  G�O�G�O�G�O�                
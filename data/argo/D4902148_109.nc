CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-08-11T18:35:51Z creation;2017-08-11T18:35:53Z conversion to V3.1;2019-12-18T07:28:54Z update;2022-11-21T05:32:10Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20170811183551  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               mA   JA  I1_0397_109                     2C  Dd1�NAVIS_A                         0397                            ARGO 011514                     863 @��
��1   @����Y @;��\)�d1��q�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  Aa��A���A���A�  A�  A�  A�33A�  A���B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Ǯ@8��@x��@�z�@�z�A=qA>=qA_�
A�
A��A��A��A��A�Q�A��A��A��B�\B�\B�\B(�B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)�=C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci�qCk��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�yH11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�-A�I�A�I�A�M�A�O�A�O�A�VA�VA�VA�VA�5?A��AڑhA���AП�A�JA��
A�K�A���A��TA��
A���A��hA��A��A�A�1'A�7LA���A�p�A�^5A�`BA���A��-A�A�A�t�A��A��#A��A��
A�ZA�~�A���A��A�p�A��#A�I�A�%A�p�A��mA�S�A���A���A�ZA��`A�{A�JA���A�VA�9XA�{A�bA���A��A��A�z�A�oA��A�JA��A�&�A�  A��A�z�A�7LA��mA��uA��;A�Q�A`BA~�`A~JA};dA{��A{XAz�DAx��Av1'Au�AtbAs�mAt  AtAs�mAs��AsƨAs��As�^As7LArffAq?}ApE�Ao�An�An1'Amt�Ak�TAj�uAg�-Ae��Aex�Ac�Aa�;Aa
=A_�A^��A\M�AZĜAY��AX��AW��AW+AV��AUS�AS�^AR��ARJAQhsAP��AP�RAPM�AO�7ANr�AM��AM
=AL5?AK��AK�AJ�AJ��AJbNAH��AF��AF�ADȴACdZAC"�ACVAB�AB�DAA�AAoA?�
A>ffA<��A;��A;oA:��A:�DA:A�A9�A9A9��A9l�A9"�A8��A7�#A7�hA6�uA5�A5dZA4��A3x�A2�A2JA1��A1VA0M�A/�FA/7LA/�A.��A.�9A.=qA.A-�PA-+A,M�A+�7A+;dA*��A*�!A*A)�hA(�9A'7LA&v�A%?}A$n�A$  A#��A"�A"-A!A!S�A ��A 9XA\)AI�A�hAA��A&�Al�A��A�-A �A�PA7LAA�HA��A��AM�AA�wAS�A��A5?At�A
=AbNA�-AĜA �A�FA;dA�uA1AXA
�A	�7A�A�AK�A�jA=qAA�FAhsA"�AĜA�A��A �@��@��+@�G�@��@�ff@�x�@���@�ȴ@�ff@�$�@��@�hs@�@���@�+@��D@��@��`@�w@��@�J@���@��@�$�@ܣ�@۝�@�C�@٩�@֧�@��#@Դ9@�S�@���@�(�@ʸR@�{@��/@Ǿw@�"�@ź^@�A�@Ý�@§�@��@���@�v�@�`B@�&�@��@���@�j@�A�@�b@���@��m@��F@�dZ@��@�ȴ@�5?@�Z@���@���@�bN@�$�@�?}@��u@�ƨ@��@�Ĝ@�@�v�@�M�@�@���@�&�@���@�z�@���@��@�5?@��@���@��@��-@���@���@�$�@�p�@�%@��`@��@��D@�1'@���@���@�5?@�%@�(�@���@�7L@�9X@�ƨ@�K�@��\@�J@��#@��#@��^@�O�@���@���@�  @�ƨ@�dZ@���@��@�/@��u@�Z@�Q�@�A�@��@��H@���@��@�j@�\)@�o@��@��!@�~�@��@�x�@��@���@��@��j@��@���@��j@���@��`@�Ĝ@�r�@�(�@��w@�C�@�
=@��@���@�{@�hs@�V@��/@�z�@�b@��;@��
@�ƨ@���@�n�@�J@��@��T@���@���@�V@�Ĝ@�z�@�(�@��;@�ƨ@�ƨ@��F@���@��P@�|�@�;d@��y@��+@�v�@�-@��@�@��h@�&�@���@���@��`@��@�1'@�@|�@\)@~ȴ@~$�@}�T@}�h@}V@{�F@{o@zn�@z�@y�@y��@x�u@w\)@vȴ@vE�@u`B@uO�@u�@u`B@tj@s�m@s��@st�@s@r�@r��@r��@r�\@rn�@rM�@r-@rJ@q�#@q��@q�7@p�`@p  @o�@nȴ@n�+@nV@n5?@m�@m��@m�h@m�@mO�@m?}@m?}@m?}@m?}@m/@m�@m�@mV@l��@l�@l�/@l�j@l��@lz�@lI�@l�@k�F@k"�@j�!@j~�@j=q@i��@ihs@i%@h�u@h1'@hb@g��@g|�@fȴ@eO�@d�j@c��@cC�@b�H@b~�@b�@a�@a�#@a��@ax�@a&�@`Q�@_�@^@]�@]?}@\�j@Z�@Z�!@Y�@X�u@Xr�@XQ�@W��@WK�@W��@W�@W�;@X�@X�u@XbN@XA�@X �@W�P@W;d@W+@W+@V��@U��@T��@T��@Tj@S�@R�H@Rn�@RJ@Q�@Q��@P��@P�u@P1'@P �@P �@P �@P1'@P1'@P1'@P �@P  @O�w@O|�@Ol�@O+@N��@N��@N�+@N$�@N@N@N@M�T@M��@MO�@L��@L��@MV@M�@MV@L��@Lz�@LZ@K��@K�F@K��@KS�@Ko@J�H@J�!@J~�@Jn�@J-@J-@J�@I��@I��@IX@H�`@HĜ@H�@HA�@H  @G��@Gl�@F�R@FE�@E�T@E�-@E��@E�h@E�@E`B@E/@EV@D�@D�j@D�D@DI�@D1@C�m@Cƨ@Ct�@Co@B�H@B��@B�!@BM�@B-@B�@BJ@BJ@A�@A�^@A��@A��@AX@@��@@ �@?|�@?;d@?�@>�y@>�R@>��@>ff@=@=/@<�@<Z@;�m@;dZ@;@:��@:�@9�7@9G�@9%@8Ĝ@8�9@8�9@8��@8�@8bN@8Q�@8A�@7�@7�P@7;d@7K�@7;d@7
=@6�y@6�@6ȴ@6�R@6�R@6�R@6v�@6E�@6$�@5�T@5@5`B@5V@4��@3ƨ@3�@3dZ@3"�@3@2��@2^5@2=q@2�@1��@1�#@1�^@1��@1��@1x�@1x�@1hs@1&�@1%@0�`@0��@0bN@0Q�@01'@0 �@/�w@/�@/K�@/
=@.5?@-�T@-��@-�-@-�h@-V@,j@,Z@,9X@,(�@,(�@,�@,�@,1@+�F@+��@+�@+S�@+S�@*�@*��@*��@*��@*�!@*=q@)�@)x�@)hs@)hs@)X@)G�@)&�@(��@(1'@'�;@'�;@'��@'��@'�w@'�w@'�@'|�@&��@&E�@&{@%�@%�@$�@$z�@$I�@$(�@$9X@$�@#�F@#S�@"�H@"��@"�\@"~�@"~�@"~�@"~�@"M�@"-@!�@!�7@!hs@!X@!7L@!�@!%@!%@!%@ ��@  �@   @�@�@�P@K�@+@�@�y@�R@ff@$�@@�@�T@��@?}@V@�@�@�@�/@�j@��@j@9X@�m@�m@��@"�@o@��@�!@��@~�@-@��@�#@��@�7@x�@hs@��@�9@r�@A�@  @��@��@�R@�+@5?@$�@@�@�@�T@��@@�@V@�@��@��@j@Z@�@1@�m@�F@C�@��@�\@=q@�@��@x�@G�@G�@G�@G�@&�@%@Ĝ@�@bN@bN@bN@Q�@ �@�;@l�@K�@+@�y@�@�R@�+@{@��@��@�h@p�@?}@/@�@�@�@V@��@�/@�j@�D@j@9X@��@33@
�\@	�#@	��@	�7@	7L@	%@Ĝ@�9@�u@��@��@��@r�@��@l�@
=@ȴ@ȴ@�+@$�@��@@�-@��@��@�@O�@��@�/@�@�D@Z@9X@��@��@�m@ƨ@�F@��@�@t�@dZ@dZ@dZ@dZ@S�@S�@C�@33@@�@�@��@�\@n�@M�@=q@=q@-@�@�^@hs@G�@&�@�@%@ �911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�-A�I�A�I�A�M�A�O�A�O�A�VA�VA�VA�VA�5?A��AڑhA���AП�A�JA��
A�K�A���A��TA��
A���A��hA��A��A�A�1'A�7LA���A�p�A�^5A�`BA���A��-A�A�A�t�A��A��#A��A��
A�ZA�~�A���A��A�p�A��#A�I�A�%A�p�A��mA�S�A���A���A�ZA��`A�{A�JA���A�VA�9XA�{A�bA���A��A��A�z�A�oA��A�JA��A�&�A�  A��A�z�A�7LA��mA��uA��;A�Q�A`BA~�`A~JA};dA{��A{XAz�DAx��Av1'Au�AtbAs�mAt  AtAs�mAs��AsƨAs��As�^As7LArffAq?}ApE�Ao�An�An1'Amt�Ak�TAj�uAg�-Ae��Aex�Ac�Aa�;Aa
=A_�A^��A\M�AZĜAY��AX��AW��AW+AV��AUS�AS�^AR��ARJAQhsAP��AP�RAPM�AO�7ANr�AM��AM
=AL5?AK��AK�AJ�AJ��AJbNAH��AF��AF�ADȴACdZAC"�ACVAB�AB�DAA�AAoA?�
A>ffA<��A;��A;oA:��A:�DA:A�A9�A9A9��A9l�A9"�A8��A7�#A7�hA6�uA5�A5dZA4��A3x�A2�A2JA1��A1VA0M�A/�FA/7LA/�A.��A.�9A.=qA.A-�PA-+A,M�A+�7A+;dA*��A*�!A*A)�hA(�9A'7LA&v�A%?}A$n�A$  A#��A"�A"-A!A!S�A ��A 9XA\)AI�A�hAA��A&�Al�A��A�-A �A�PA7LAA�HA��A��AM�AA�wAS�A��A5?At�A
=AbNA�-AĜA �A�FA;dA�uA1AXA
�A	�7A�A�AK�A�jA=qAA�FAhsA"�AĜA�A��A �@��@��+@�G�@��@�ff@�x�@���@�ȴ@�ff@�$�@��@�hs@�@���@�+@��D@��@��`@�w@��@�J@���@��@�$�@ܣ�@۝�@�C�@٩�@֧�@��#@Դ9@�S�@���@�(�@ʸR@�{@��/@Ǿw@�"�@ź^@�A�@Ý�@§�@��@���@�v�@�`B@�&�@��@���@�j@�A�@�b@���@��m@��F@�dZ@��@�ȴ@�5?@�Z@���@���@�bN@�$�@�?}@��u@�ƨ@��@�Ĝ@�@�v�@�M�@�@���@�&�@���@�z�@���@��@�5?@��@���@��@��-@���@���@�$�@�p�@�%@��`@��@��D@�1'@���@���@�5?@�%@�(�@���@�7L@�9X@�ƨ@�K�@��\@�J@��#@��#@��^@�O�@���@���@�  @�ƨ@�dZ@���@��@�/@��u@�Z@�Q�@�A�@��@��H@���@��@�j@�\)@�o@��@��!@�~�@��@�x�@��@���@��@��j@��@���@��j@���@��`@�Ĝ@�r�@�(�@��w@�C�@�
=@��@���@�{@�hs@�V@��/@�z�@�b@��;@��
@�ƨ@���@�n�@�J@��@��T@���@���@�V@�Ĝ@�z�@�(�@��;@�ƨ@�ƨ@��F@���@��P@�|�@�;d@��y@��+@�v�@�-@��@�@��h@�&�@���@���@��`@��@�1'@�@|�@\)@~ȴ@~$�@}�T@}�h@}V@{�F@{o@zn�@z�@y�@y��@x�u@w\)@vȴ@vE�@u`B@uO�@u�@u`B@tj@s�m@s��@st�@s@r�@r��@r��@r�\@rn�@rM�@r-@rJ@q�#@q��@q�7@p�`@p  @o�@nȴ@n�+@nV@n5?@m�@m��@m�h@m�@mO�@m?}@m?}@m?}@m?}@m/@m�@m�@mV@l��@l�@l�/@l�j@l��@lz�@lI�@l�@k�F@k"�@j�!@j~�@j=q@i��@ihs@i%@h�u@h1'@hb@g��@g|�@fȴ@eO�@d�j@c��@cC�@b�H@b~�@b�@a�@a�#@a��@ax�@a&�@`Q�@_�@^@]�@]?}@\�j@Z�@Z�!@Y�@X�u@Xr�@XQ�@W��@WK�@W��@W�@W�;@X�@X�u@XbN@XA�@X �@W�P@W;d@W+@W+@V��@U��@T��@T��@Tj@S�@R�H@Rn�@RJ@Q�@Q��@P��@P�u@P1'@P �@P �@P �@P1'@P1'@P1'@P �@P  @O�w@O|�@Ol�@O+@N��@N��@N�+@N$�@N@N@N@M�T@M��@MO�@L��@L��@MV@M�@MV@L��@Lz�@LZ@K��@K�F@K��@KS�@Ko@J�H@J�!@J~�@Jn�@J-@J-@J�@I��@I��@IX@H�`@HĜ@H�@HA�@H  @G��@Gl�@F�R@FE�@E�T@E�-@E��@E�h@E�@E`B@E/@EV@D�@D�j@D�D@DI�@D1@C�m@Cƨ@Ct�@Co@B�H@B��@B�!@BM�@B-@B�@BJ@BJ@A�@A�^@A��@A��@AX@@��@@ �@?|�@?;d@?�@>�y@>�R@>��@>ff@=@=/@<�@<Z@;�m@;dZ@;@:��@:�@9�7@9G�@9%@8Ĝ@8�9@8�9@8��@8�@8bN@8Q�@8A�@7�@7�P@7;d@7K�@7;d@7
=@6�y@6�@6ȴ@6�R@6�R@6�R@6v�@6E�@6$�@5�T@5@5`B@5V@4��@3ƨ@3�@3dZ@3"�@3@2��@2^5@2=q@2�@1��@1�#@1�^@1��@1��@1x�@1x�@1hs@1&�@1%@0�`@0��@0bN@0Q�@01'@0 �@/�w@/�@/K�@/
=@.5?@-�T@-��@-�-@-�h@-V@,j@,Z@,9X@,(�@,(�@,�@,�@,1@+�F@+��@+�@+S�@+S�@*�@*��@*��@*��@*�!@*=q@)�@)x�@)hs@)hs@)X@)G�@)&�@(��@(1'@'�;@'�;@'��@'��@'�w@'�w@'�@'|�@&��@&E�@&{@%�@%�@$�@$z�@$I�@$(�@$9X@$�@#�F@#S�@"�H@"��@"�\@"~�@"~�@"~�@"~�@"M�@"-@!�@!�7@!hs@!X@!7L@!�@!%@!%@!%@ ��@  �@   @�@�@�P@K�@+@�@�y@�R@ff@$�@@�@�T@��@?}@V@�@�@�@�/@�j@��@j@9X@�m@�m@��@"�@o@��@�!@��@~�@-@��@�#@��@�7@x�@hs@��@�9@r�@A�@  @��@��@�R@�+@5?@$�@@�@�@�T@��@@�@V@�@��@��@j@Z@�@1@�m@�F@C�@��@�\@=q@�@��@x�@G�@G�@G�@G�@&�@%@Ĝ@�@bN@bN@bN@Q�@ �@�;@l�@K�@+@�y@�@�R@�+@{@��@��@�h@p�@?}@/@�@�@�@V@��@�/@�j@�D@j@9X@��@33@
�\@	�#@	��@	�7@	7L@	%@Ĝ@�9@�u@��@��@��@r�@��@l�@
=@ȴ@ȴ@�+@$�@��@@�-@��@��@�@O�@��@�/@�@�D@Z@9X@��@��@�m@ƨ@�F@��@�@t�@dZ@dZ@dZ@dZ@S�@S�@C�@33@@�@�@��@�\@n�@M�@=q@=q@-@�@�^@hs@G�@&�@�@%@ �911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��B�B�B�B�B�B�B�B�B�B�B�B��B��B�B�B��B��B��B��B��B��B�yB�NB��B��BǮB�wB�FB�-B�!B�B��B��B��B��B��B�\B� Bt�Bl�BYBD�B?}BA�B@�B;dB8RB49B0!B+B'�B!�B�B�B\BB��B�`B��B�9B��B�1B]/BM�BE�B<jB�BJB  B
�B
�;B
��B
�qB
�RB
�3B
�B
��B
��B
�hB
�JB
�%B
~�B
u�B
o�B
iyB
\)B
G�B
@�B
9XB
9XB
:^B
;dB
:^B
9XB
9XB
8RB
9XB
5?B
0!B
)�B
$�B
!�B
�B
�B
�B
JB
B	��B	�B	�fB	�/B	��B	��B	ÖB	�dB	�!B	��B	��B	��B	��B	�{B	�bB	�+B	}�B	v�B	u�B	s�B	q�B	o�B	m�B	hsB	cTB	_;B	[#B	VB	Q�B	M�B	M�B	M�B	J�B	@�B	7LB	49B	.B	'�B	&�B	&�B	%�B	$�B	!�B	�B	�B	hB	+B	  B��B��B��B��B��B��B��B�B�B�B�B�B�sB�`B�TB�BB�/B�B�B�B��B��B��B��B��B��B��BȴBǮBŢBÖB��B�qB�dB�^B�XB�FB�9B�!B�B��B��B��B��B��B��B��B��B��B�uB�hB�\B�PB�=B�1B�%B�B|�By�Bv�Br�Bp�Bp�Bo�Bo�Bn�Bm�Bl�Bk�BjBiyBgmBe`BcTBbNB`BB^5B\)BZBXBW
BT�BR�BP�BM�BK�BI�BG�BF�BE�BD�BC�BB�BA�B?}B=qB;dB8RB5?B33B1'B/B.B-B+B)�B(�B(�B(�B'�B&�B%�B$�B!�B�B�B�B�B�B�B�B{BuBoBoBhB\BbB\BVBJBJBVBVBVBVBVBVBVB\B\B\BbBhBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B"�B'�B-B/B/B0!B1'B33B49B6FB9XB;dB=qB>wB>wB@�B?}B?}B?}B=qB@�BB�BC�BD�BE�BH�BK�BO�BS�BR�BR�B]/BhsBo�Bq�Bs�Bw�By�Bz�By�By�B{�B}�B~�B�B�B�B�+B�=B�JB�VB�bB�hB�oB��B��B��B��B�B�!B�'B�dB�wB��BB��B��B��BBÖBŢBƨBȴBɺB��B��B��B��B��B�
B�B�B�B�/B�NB�ZB�`B�yB�B�B�B�B�B��B��B��B��B��B	  B	%B	1B	DB	PB	bB	bB	bB	hB	hB	oB	oB	{B	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	&�B	'�B	,B	-B	/B	0!B	1'B	2-B	33B	5?B	7LB	;dB	=qB	>wB	?}B	?}B	@�B	@�B	C�B	C�B	C�B	D�B	I�B	J�B	L�B	O�B	R�B	T�B	VB	XB	YB	\)B	]/B	^5B	_;B	aHB	bNB	cTB	dZB	e`B	ffB	hsB	l�B	p�B	q�B	r�B	r�B	r�B	t�B	u�B	u�B	v�B	w�B	w�B	w�B	w�B	w�B	w�B	x�B	x�B	x�B	x�B	y�B	y�B	y�B	y�B	z�B	{�B	|�B	~�B	�B	�B	�B	�B	�%B	�1B	�=B	�DB	�PB	�PB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�FB	�FB	�?B	�9B	�3B	�3B	�9B	�9B	�?B	�RB	�^B	�jB	�jB	�wB	B	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�;B	�;B	�BB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B

=B

=B

=B
DB
DB
DB
DB
JB
JB
PB
PB
bB
hB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
0!B
1'B
2-B
2-B
2-B
2-B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
VB
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
ffB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
jB
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��B�B�1B�B�1B�B�B�1B�KBٚB�B��B��B�B�tB�dB�6B�B��BB��B��B�)B��B�KB̘B�=B��B��B��B�aB�AB�B��B��B��B�yB��B��Bv�Bp�B\�BF%BABB�BA�B<PB9�B5�B1�B,�B)�B"�B!B�BBEB�$B��B�eB�fB�tB��B_�BO\BG�BA�B"4B�B�B
��B
�4B
�.B
�wB
�>B
�nB
��B
�`B
�B
�TB
�jB
�EB
�iB
v�B
qB
k�B
_B
I7B
AoB
9�B
9rB
:^B
;�B
:xB
9rB
9�B
8�B
:DB
6zB
1�B
+B
%�B
"�B
�B
�B
�B
pB
?B	��B	�B	�B	ߊB	�:B	�dB	ŢB	�B	��B	�_B	��B	��B	��B	��B	�B	�B	.B	w�B	v�B	tTB	r-B	pUB	n�B	i�B	dZB	`BB	\)B	V�B	R�B	N<B	NpB	OB	MB	B�B	8lB	5�B	/�B	(sB	'8B	'RB	&�B	%�B	#:B	�B	�B	[B	�B	 �B�]B�jB�dB�XB�B�+B�%B�9B�|B�B�IB��B�B�B�B��BޞBڠB��B��B�B��BѝB�BB�(B�VB�xB�7B�fB�YB��B�oB��B�B�B�DB�LB��B��B�CB��B��B��B��B��B��B�xB�kB�yB�FB��B��B�VB�DB�7B�KB�B}�B{�Bx�Bs�BqABp�Bo�Bo�Bo Bn/BmBl"BkQBjBhXBffBd&BcnBa|B_�B]/BZ�BX�BW�BVBTBQ�BO�BL�BK)BH�BGzBFYBEBDBCBB'B@iB?.B<�B:�B6�B4B2GB0�B.�B-�B,"B*�B)yB)_B)DB(sB'�B&�B&2B#�B"NB�B�BqB�B�B�B9B{B@BB�BB4BbB�B�BVB\B�BBB(BB\B\B�BHB}BhBTBFB�B�B�B�B�B�B�B�B�B�B�BBSB�B�B�B�B/B �B!|B"�B"�B$ZB)B-wB/iB/iB0�B1�B3�B4�B7B:B<B=�B>�B>�B@iB?}B?�B?�B>B@�BB�BC�BD�BFBIBLdBQ BT�BS�BT,B^5Bi*Bp!Br-Bt9BxRBzBz�BzBz^B|PB~BB�B�{B��B��B��B��B��B��B�}B��B��B�KB�5B��B�B��B�oB�vB�B��B��B��B��B��B��B��B��B��BƨB��B��B�B�"B�4B�uBՁB�?B�_B�BڠB��B�B�B�B��B��B��B��B��B�B�$B�B�B�BB�.B	 iB	tB	�B	�B	�B	}B	}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	"4B	%B	&B	'B	'RB	(XB	,qB	-)B	/5B	0�B	1vB	2|B	3hB	5�B	7�B	;�B	=�B	>�B	?�B	?�B	@�B	@�B	C�B	C�B	C�B	D�B	I�B	KB	M6B	P.B	SB	U2B	V9B	X+B	Y1B	\)B	]IB	^jB	_pB	a|B	bhB	c�B	dtB	e�B	f�B	h�B	l�B	p�B	q�B	r�B	r�B	r�B	t�B	u�B	u�B	v�B	w�B	w�B	w�B	w�B	w�B	xB	x�B	y	B	y	B	x�B	y�B	y�B	y�B	y�B	z�B	|B	}<B	.B	�[B	�MB	�MB	�mB	�YB	��B	�rB	��B	�jB	��B	��B	��B	�B	��B	�	B	�B	�B	��B	�B	�B	�B	�B	�
B	�DB	�_B	��B	��B	�tB	��B	��B	��B	��B	��B	��B	�nB	�nB	��B	��B	�DB	�jB	��B	�]B	ªB	��B	��B	��B	�	B	��B	�B	�B	�B	�VB	�TB	�2B	�2B	�SB	�eB	�WB	�dB	�VB	�pB	��B	�B	�B	�tB	�zB	�`B	�zB	�`B	�zB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�0B	�B	�B	�B	�B	�"B	�B	�]B
;B
;B
 B
'B
'B
'B
AB
AB
GB
GB
3B
3B
3B
SB
9B
?B
?B
EB
KB
KB
KB
	RB

XB

=B

XB
^B
^B
^B
^B
dB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
"�B
#B
#B
#�B
#�B
#�B
#�B
#�B
$B
$B
$�B
$�B
$�B
%B
%�B
&2B
&2B
(
B
)*B
)*B
)*B
)DB
*0B
*B
+B
+B
+6B
+6B
,"B
,B
,=B
,"B
,"B
,=B
-)B
-)B
-CB
-)B
./B
./B
./B
.IB
/OB
/OB
/OB
0�B
1[B
2GB
2GB
2aB
2|B
3hB
49B
4nB
5?B
5ZB
5ZB
5?B
5tB
5ZB
5tB
6`B
6`B
6zB
6zB
7�B
7fB
7LB
7�B
7�B
8lB
8�B
:xB
:^B
:^B
:�B
:xB
;�B
<�B
<�B
=�B
=qB
=�B
=qB
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
NB
NB
NB
OB
OB
O�B
OB
PB
QB
Q B
Q B
Q B
QB
R B
RB
RB
R B
R�B
S&B
SB
S@B
S&B
TB
T,B
T,B
T,B
U2B
VB
W?B
W?B
X+B
XEB
X+B
XB
XB
X+B
X+B
XEB
X_B
Y1B
ZB
Z7B
[=B
[=B
[WB
\)B
\CB
\CB
\]B
\]B
]dB
]dB
]dB
]IB
]dB
]IB
^OB
^OB
^OB
^OB
^jB
^OB
_pB
_VB
_VB
_;B
`\B
`vB
`vB
abB
abB
abB
a|B
bNB
b�B
bhB
c�B
c�B
c�B
cnB
cnB
dtB
dZB
dtB
dZB
dZB
dZB
dtB
dtB
d�B
dtB
dtB
dtB
d�B
d�B
e�B
f�B
g�B
g�B
g�B
h�B
h�B
i�B
iyB
j�B
j�B
j�B
j�B
j�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<XD�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201708220033192017082200331920170822003319202211182131262022111821312620221118213126201804031936512018040319365120180403193651  JA  ARFMdecpA19c                                                                20170812033515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170811183551  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170811183552  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170811183552  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170811183553  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170811183553  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170811183553  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170811183553  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170811183553  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170811183553                      G�O�G�O�G�O�                JA  ARUP                                                                        20170811185646                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170811153247  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20170821153319  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170821153319  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103651  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171535                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123126  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                
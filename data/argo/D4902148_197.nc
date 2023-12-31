CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-08T15:37:48Z creation;2020-01-08T15:37:53Z conversion to V3.1;2022-11-21T05:27:45Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ά   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200108153748  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_197                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @����ƀ1   @�����Q�@<6�+J�dz���>B1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @\)@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮBÔ{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D�\Dx�D��Dx�D��Dr�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>r�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�?�D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A���A��A�l�A�A�A��TA�A���A��A�{A�7LA��!A��RA�oA�VA�v�A��TA�~�A�9XA�t�A��+A���A��mA�oA�33A��jA���A��A��A��RA���A��HA���A�  A�O�A���A�I�A�33A��A�K�A�/A�%A�ZA��A�ffA��A�5?A�G�A�ZA���A�9XA��A�ȴA�bNA��HA��\A�M�A���A��^A��A���A�1'A��`A�1'A�$�A�JA��A���A�"�A�?}A�ZA�%A��!A���A~jA|��A{��Az�HAy��AyVAw�At��ArVAq�Ao��Ao;dAn1'AmO�Ak`BAg�Ae��Ad��Ac��Ab��Ab��Ab=qAa��AaXA`r�A_�hA^�jA^r�A]�TA]
=A[dZAZn�AX�uAVVAU�
AU��AUAT$�AS?}ARE�AQ33APbAO�wAN�/AM�7AL��AKhsAH�jAGƨAF��AE��AC��AC%AB��ABr�AA��A@��A?��A?&�A>�HA>r�A=��A<1'A9�;A7�^A6�RA6�A5��A4��A3`BA2z�A0{A/?}A.�9A.Q�A-�;A-��A,��A+�A*��A*r�A)�wA'�#A&�uA%�;A%��A%%A#��A!�TA!�A��A��A��A�A��A�Ax�AXA�Az�A-A�-A"�A�7A��A �A��A�A�;A�9A(�A��AZA�A|�AS�A7LA+A��A�9A5?A�wAA�A�^A�9A �A�A
(�A	dZA$�A�FA��AK�A��A=qAbA�TA�A?}A�A~�A^5A9XA�AQ�A+@��@���@��@�\)@��H@��\@��#@���@���@��@��
@��H@�+@�G�@�@�\@�p�@�w@���@��`@��@�bN@��@�G�@�Q�@�\)@�ff@�$�@ٲ-@�r�@�t�@��@�&�@�  @��@���@�  @�;d@�&�@���@Ǯ@�S�@�@ƸR@��@�G�@�Q�@��;@�|�@�;d@¸R@��@��@�V@��@��R@�=q@�?}@���@���@��@���@�X@���@�9X@��@���@���@��`@� �@��@�dZ@�~�@��@��7@�7L@���@���@���@��@�J@��-@�O�@��@�r�@�I�@�1@���@�+@�@��y@��\@�J@���@���@�?}@�  @�\)@�7L@��w@�|�@��@�J@��@�?}@��9@�b@��@�dZ@��H@��\@�ff@�^5@�ff@��T@�r�@�o@���@�O�@�9X@��@���@�J@��7@��@��@�j@�1'@�b@���@���@���@��y@��\@��\@���@�~�@��^@��-@��@��T@���@��@��@�l�@�K�@�C�@�+@��@���@��@��@��@��@��#@���@���@�r�@���@�C�@�"�@��@�
=@���@��R@�V@��7@�&�@��/@��@��;@��P@�dZ@�33@���@�ƨ@���@�\)@���@�E�@�$�@�@�@�p�@�G�@���@�9X@�\)@�o@��y@���@�ff@��^@�G�@��@��j@���@��D@�j@�A�@��@��@|�@K�@~��@~�@~ȴ@~�R@~��@~@}��@|�/@|Z@{��@{33@y�#@yX@x��@x�9@x�@x1'@x  @w��@w��@w|�@w
=@v�@v��@vff@u`B@s��@sC�@r��@r��@rM�@r-@rJ@q�^@qX@p�9@p  @o��@ol�@o;d@n��@nv�@n{@m��@m?}@l�@l�@l��@l�@m�@l��@l9X@l�@l1@k�
@kƨ@k�F@kt�@kS�@k33@k@j��@jM�@i�@ix�@h�9@hb@g��@f��@fE�@e�T@e�-@e��@e��@e`B@eV@dI�@c��@co@b�!@b~�@b^5@bM�@bJ@a�#@a�^@a�7@ax�@aX@aG�@a7L@a7L@`��@`�9@`��@`��@`�@`bN@`b@_�w@_
=@^��@^v�@^E�@]�T@]p�@]O�@\��@\�@\�D@\�D@\9X@[�
@[�@[S�@["�@[o@[@Z�H@Z��@ZM�@Z=q@Y��@Y7L@X��@XA�@W�w@WK�@Vȴ@V5?@U�T@U�-@T�@T�j@Tj@T9X@S�@R��@Rn�@R=q@Q�@Q��@Q��@Qhs@Q7L@Q%@P��@PQ�@O�@O;d@O
=@Nȴ@N�R@N��@N��@N5?@M�@Mp�@M/@MV@L��@L�@K�
@Kt�@KC�@K@Jn�@JM�@I��@H��@H�u@H1'@G�;@G�w@G|�@Gl�@GK�@G;d@G;d@G
=@Fȴ@F�+@Fv�@FV@FE�@F$�@F{@E�T@E�h@E/@D�@D��@D�@Dj@D�@D1@Cƨ@Co@B��@B^5@A��@Ax�@AG�@A7L@A�@@��@@�9@@1'@@  @?��@?�@?�P@?l�@?K�@?�@>��@>�R@>�+@>V@=�@=�h@=p�@=`B@=O�@=/@<�/@<j@<1@;ƨ@;t�@;33@:��@:J@9�7@97L@9%@8�9@8bN@8  @7�;@7�@7�w@7�@7�@6�R@6ff@65?@6$�@6{@5�-@5�@4��@4�@4�@4�j@4�@4j@41@3�m@3��@3t�@3C�@2�!@2�\@2�\@2n�@2M�@2=q@1�#@1�^@1G�@1%@0�9@0�u@0�@0r�@0bN@0A�@0  @/�;@/�P@/+@.��@.ȴ@.$�@-�@-��@-p�@-O�@-/@,��@,�@,z�@,Z@,I�@,1@+��@*�H@*��@*~�@*^5@*=q@*-@)��@)x�@)X@(��@(1'@'�;@'�w@'��@'\)@'
=@&�R@&��@&�+@&v�@&v�@&V@%��@%�@$�@$Z@#��@#�m@#�m@#�F@#�F@#�F@#�F@#��@#�@#�@#�@#S�@#C�@#33@"�H@"�\@"M�@"�@!��@!�7@!7L@!&�@!7L@!&�@!&�@!&�@!&�@!�@ r�@�@��@�@�P@l�@+@��@��@5?@�@�@��@�/@��@�j@�@��@��@�D@z�@Z@1@�@C�@o@�H@��@�@��@hs@7L@�@%@��@Ĝ@�9@�9@��@��@��@�u@�u@r�@�@\)@�@ȴ@�R@5?@@�@@��@p�@�@�j@��@j@�@�m@ƨ@��@�@S�@"�@@�H@�!@~�@n�@^5@^5@^5@^5@-@�#@hs@G�@7L@&�@�@��@�`@�`@��@Ĝ@�9@��@r�@A�@ �@b@b@  @�@�;@��@��@�w@�P@l�@\)@��@�@v�@5?@$�@@�@�T@��@�-@��@�h@�@�@�@�@�@�@�@p�@p�@p�@/@�@�@��@I�@1@�
@ƨ@�F@�@dZ@"�@
�H@
��@
�!@
~�@
n�@
M�@
-@
J@	�@	�@	��@	��@	��@	�7@	x�@	X@	7L@	7L@	&�@	&�@	�@	�@	�@	�@��@�9@�u@bN@Q�@A�@1'@1'@ �@�w@
=@�@�R@�R@�R@��@v�@E�@5?@@�T@�h@?}@�@�@I�@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A���A��A�l�A�A�A��TA�A���A��A�{A�7LA��!A��RA�oA�VA�v�A��TA�~�A�9XA�t�A��+A���A��mA�oA�33A��jA���A��A��A��RA���A��HA���A�  A�O�A���A�I�A�33A��A�K�A�/A�%A�ZA��A�ffA��A�5?A�G�A�ZA���A�9XA��A�ȴA�bNA��HA��\A�M�A���A��^A��A���A�1'A��`A�1'A�$�A�JA��A���A�"�A�?}A�ZA�%A��!A���A~jA|��A{��Az�HAy��AyVAw�At��ArVAq�Ao��Ao;dAn1'AmO�Ak`BAg�Ae��Ad��Ac��Ab��Ab��Ab=qAa��AaXA`r�A_�hA^�jA^r�A]�TA]
=A[dZAZn�AX�uAVVAU�
AU��AUAT$�AS?}ARE�AQ33APbAO�wAN�/AM�7AL��AKhsAH�jAGƨAF��AE��AC��AC%AB��ABr�AA��A@��A?��A?&�A>�HA>r�A=��A<1'A9�;A7�^A6�RA6�A5��A4��A3`BA2z�A0{A/?}A.�9A.Q�A-�;A-��A,��A+�A*��A*r�A)�wA'�#A&�uA%�;A%��A%%A#��A!�TA!�A��A��A��A�A��A�Ax�AXA�Az�A-A�-A"�A�7A��A �A��A�A�;A�9A(�A��AZA�A|�AS�A7LA+A��A�9A5?A�wAA�A�^A�9A �A�A
(�A	dZA$�A�FA��AK�A��A=qAbA�TA�A?}A�A~�A^5A9XA�AQ�A+@��@���@��@�\)@��H@��\@��#@���@���@��@��
@��H@�+@�G�@�@�\@�p�@�w@���@��`@��@�bN@��@�G�@�Q�@�\)@�ff@�$�@ٲ-@�r�@�t�@��@�&�@�  @��@���@�  @�;d@�&�@���@Ǯ@�S�@�@ƸR@��@�G�@�Q�@��;@�|�@�;d@¸R@��@��@�V@��@��R@�=q@�?}@���@���@��@���@�X@���@�9X@��@���@���@��`@� �@��@�dZ@�~�@��@��7@�7L@���@���@���@��@�J@��-@�O�@��@�r�@�I�@�1@���@�+@�@��y@��\@�J@���@���@�?}@�  @�\)@�7L@��w@�|�@��@�J@��@�?}@��9@�b@��@�dZ@��H@��\@�ff@�^5@�ff@��T@�r�@�o@���@�O�@�9X@��@���@�J@��7@��@��@�j@�1'@�b@���@���@���@��y@��\@��\@���@�~�@��^@��-@��@��T@���@��@��@�l�@�K�@�C�@�+@��@���@��@��@��@��@��#@���@���@�r�@���@�C�@�"�@��@�
=@���@��R@�V@��7@�&�@��/@��@��;@��P@�dZ@�33@���@�ƨ@���@�\)@���@�E�@�$�@�@�@�p�@�G�@���@�9X@�\)@�o@��y@���@�ff@��^@�G�@��@��j@���@��D@�j@�A�@��@��@|�@K�@~��@~�@~ȴ@~�R@~��@~@}��@|�/@|Z@{��@{33@y�#@yX@x��@x�9@x�@x1'@x  @w��@w��@w|�@w
=@v�@v��@vff@u`B@s��@sC�@r��@r��@rM�@r-@rJ@q�^@qX@p�9@p  @o��@ol�@o;d@n��@nv�@n{@m��@m?}@l�@l�@l��@l�@m�@l��@l9X@l�@l1@k�
@kƨ@k�F@kt�@kS�@k33@k@j��@jM�@i�@ix�@h�9@hb@g��@f��@fE�@e�T@e�-@e��@e��@e`B@eV@dI�@c��@co@b�!@b~�@b^5@bM�@bJ@a�#@a�^@a�7@ax�@aX@aG�@a7L@a7L@`��@`�9@`��@`��@`�@`bN@`b@_�w@_
=@^��@^v�@^E�@]�T@]p�@]O�@\��@\�@\�D@\�D@\9X@[�
@[�@[S�@["�@[o@[@Z�H@Z��@ZM�@Z=q@Y��@Y7L@X��@XA�@W�w@WK�@Vȴ@V5?@U�T@U�-@T�@T�j@Tj@T9X@S�@R��@Rn�@R=q@Q�@Q��@Q��@Qhs@Q7L@Q%@P��@PQ�@O�@O;d@O
=@Nȴ@N�R@N��@N��@N5?@M�@Mp�@M/@MV@L��@L�@K�
@Kt�@KC�@K@Jn�@JM�@I��@H��@H�u@H1'@G�;@G�w@G|�@Gl�@GK�@G;d@G;d@G
=@Fȴ@F�+@Fv�@FV@FE�@F$�@F{@E�T@E�h@E/@D�@D��@D�@Dj@D�@D1@Cƨ@Co@B��@B^5@A��@Ax�@AG�@A7L@A�@@��@@�9@@1'@@  @?��@?�@?�P@?l�@?K�@?�@>��@>�R@>�+@>V@=�@=�h@=p�@=`B@=O�@=/@<�/@<j@<1@;ƨ@;t�@;33@:��@:J@9�7@97L@9%@8�9@8bN@8  @7�;@7�@7�w@7�@7�@6�R@6ff@65?@6$�@6{@5�-@5�@4��@4�@4�@4�j@4�@4j@41@3�m@3��@3t�@3C�@2�!@2�\@2�\@2n�@2M�@2=q@1�#@1�^@1G�@1%@0�9@0�u@0�@0r�@0bN@0A�@0  @/�;@/�P@/+@.��@.ȴ@.$�@-�@-��@-p�@-O�@-/@,��@,�@,z�@,Z@,I�@,1@+��@*�H@*��@*~�@*^5@*=q@*-@)��@)x�@)X@(��@(1'@'�;@'�w@'��@'\)@'
=@&�R@&��@&�+@&v�@&v�@&V@%��@%�@$�@$Z@#��@#�m@#�m@#�F@#�F@#�F@#�F@#��@#�@#�@#�@#S�@#C�@#33@"�H@"�\@"M�@"�@!��@!�7@!7L@!&�@!7L@!&�@!&�@!&�@!&�@!�@ r�@�@��@�@�P@l�@+@��@��@5?@�@�@��@�/@��@�j@�@��@��@�D@z�@Z@1@�@C�@o@�H@��@�@��@hs@7L@�@%@��@Ĝ@�9@�9@��@��@��@�u@�u@r�@�@\)@�@ȴ@�R@5?@@�@@��@p�@�@�j@��@j@�@�m@ƨ@��@�@S�@"�@@�H@�!@~�@n�@^5@^5@^5@^5@-@�#@hs@G�@7L@&�@�@��@�`@�`@��@Ĝ@�9@��@r�@A�@ �@b@b@  @�@�;@��@��@�w@�P@l�@\)@��@�@v�@5?@$�@@�@�T@��@�-@��@�h@�@�@�@�@�@�@�@p�@p�@p�@/@�@�@��@I�@1@�
@ƨ@�F@�@dZ@"�@
�H@
��@
�!@
~�@
n�@
M�@
-@
J@	�@	�@	��@	��@	��@	�7@	x�@	X@	7L@	7L@	&�@	&�@	�@	�@	�@	�@��@�9@�u@bN@Q�@A�@1'@1'@ �@�w@
=@�@�R@�R@�R@��@v�@E�@5?@@�T@�h@?}@�@�@I�@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bn�Bm�Bn�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bu�B|�B}�B�B�%B�7B�1B�B}�Bz�Bv�Bn�BhsBaHB[#BXBS�BQ�By�Bu�Bk�Bk�BdZB_;B[#BVBN�BE�B<jB5?B0!B-B(�B"�B�BDB�B�HB��B��B��B�}B��B�BM�B@�BI�BVBS�BL�BH�BE�BA�B9XB49B/B,B'�B�B�BhB
��B
��B
��B
�B
�yB
�/B
��B
ŢB
�RB
�-B
�B
��B
�B
y�B
o�B
hsB
aHB
YB
K�B
:^B
)�B
!�B
�B
{B
PB
B	��B	�BB	��B	��B	ǮB	ĜB	��B	�}B	�qB	�XB	�3B	�B	��B	��B	��B	��B	��B	�VB	�B	y�B	w�B	u�B	r�B	m�B	gmB	bNB	]/B	XB	T�B	O�B	I�B	E�B	?}B	5?B	0!B	,B	&�B	�B	�B	�B	�B	uB	VB	
=B	1B	%B	B	B��B�B�BB�#B�B��B��BɺBŢB�jB�RB�FB�?B�3B�'B�B��B��B��B��B��B��B��B�uB�hB�PB�1B�+B�B� B~�B~�B}�B}�B}�B|�B|�B{�By�Bx�Bu�Bq�Bn�Bk�BgmBe`BcTBaHB_;B]/B[#BZBZBYBYBYBXBW
BVBS�BQ�BO�BM�BK�BI�BH�BH�BF�BE�BE�BD�BD�BC�BC�BC�BC�BB�BB�BB�BA�BA�B@�B>wB=qB;dB9XB8RB7LB7LB6FB6FB5?B49B2-B2-B1'B1'B0!B/B/B.B-B,B(�B)�B)�B)�B,B,B,B,B-B,B,B-B-B-B.B-B.B/B0!B/B0!B1'B5?B5?B5?B5?B5?B7LB8RB8RB9XB9XB9XB9XB:^B:^B;dB=qB=qB>wB@�BC�BD�BD�BF�BG�BG�BH�BJ�BJ�BL�BO�BP�BS�BW
BXBYBZB\)B\)B_;BbNBe`BffBffBhsBiyBjBk�Bk�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bn�Bp�Bq�Bv�B�B�B�B�1B�7B�DB�JB�PB�VB�\B�bB�bB�bB�hB�oB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�-B�RB�^B�dB�dB�dBBĜBĜBŢB��B��B��B��B��B��B��B��B�B�B�B�B�B�#B�5B�;B�TB�ZB�sB�yB�yB�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	
=B	PB	VB	\B	bB	hB	hB	uB	�B	�B	�B	�B	�B	!�B	(�B	+B	-B	.B	/B	0!B	1'B	2-B	49B	6FB	8RB	9XB	;dB	;dB	<jB	<jB	<jB	?}B	@�B	C�B	E�B	F�B	H�B	N�B	Q�B	T�B	VB	W
B	XB	YB	YB	[#B	\)B	]/B	^5B	_;B	_;B	bNB	ffB	iyB	k�B	l�B	m�B	n�B	n�B	q�B	s�B	t�B	v�B	x�B	y�B	z�B	{�B	~�B	� B	�B	�B	�+B	�1B	�=B	�JB	�VB	�hB	�uB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�?B	�FB	�FB	�LB	�LB	�LB	�RB	�XB	�XB	�^B	�dB	�jB	�qB	�wB	�wB	�wB	�wB	�wB	�}B	��B	�}B	��B	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�#B	�)B	�5B	�BB	�HB	�TB	�ZB	�`B	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
1B
1B
1B
1B
	7B
	7B

=B

=B
DB
JB
PB
PB
VB
\B
\B
\B
hB
hB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
,B
-B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
>wB
>wB
>wB
?}B
?}B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
XB
XB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
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
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bn�Bm�Bn�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bv+B}<B~�B��B��B�#B��B�B�B��Bz�BpoBkBdZB\xBYBU2BT�B}�BxBm�Bo BfLB`�B]�BYBR�BH�B>�B6�B1B.�B*�B$�B!�BvB�B��BӏB��B�B�-B�XB��BO�B@�BJ	BW$BU2BM�BIRBF�BB�B:*B4�B/�B,�B)_B!BB�B
�xB
�+B
�tB
�B
�kB
�B
�B
ǔB
��B
��B
��B
��B
��B
{dB
p�B
i�B
b�B
[�B
N�B
<�B
+�B
# B
�B
B
�B
B	��B	�B	�mB	�B	ȀB	�9B	�B	� B	�wB	�xB	�TB	� B	��B	��B	�,B	��B	�?B	��B	�_B	z�B	xlB	v�B	s�B	n�B	h�B	c�B	^�B	X�B	VSB	Q�B	K)B	G�B	BAB	6�B	1[B	-�B	)DB	�B	CB	7B	�B	�B	�B	B	�B	�B	YB	�B��B��B�|B��B�QBּBЗB�^B�KB��B�	B��B��B��B��B��B��B��B�LB�B�IB�B�9B��B�[B��B�lB�lB�mB��BcB.B~(B~(B~]B}�B}�B|�Bz�By�Bw�Br�BpBm)Bh$Bf�Bd�Bb4B`\B^�B\BZ�BZ�BYeBYKBYBX�BW�BV�BU2BS@BP�BOBBL�BKDBJ	BJ	BH1BFYBE�BE9BEmBDMBDBDBD3BC-BCGBCBA�BB'BA�B@4B?.B=qB:^B9XB8B7�B6�B7B6`B5�B3hB3B1�B1�B1AB0UB/�B/5B.�B-�B,B+�B+�B+�B,�B,�B,�B,�B-]B,�B-B-�B-�B.cB/B.B/iB0B1B0�B1�B2�B5�B5�B5�B5�B5�B8B8�B8�B9�B9�B9�B9�B:�B;JB<jB>B>]B?cBAoBC�BEBEBGEBHBHKBI�BK�BK^BMPBPHBQ�BT�BW�BX_BYeBZ�B\�B\�B`BcBe�Bf�BgBh�Bi�Bj�Bk�Bk�Bm�Bm�Bm�Bo Bo Bn�BoBo�Bq�Bs3Bw�B�UB��B��B��B��B��B��B��B��B��B��B��B��B��B�&B��B��B�1B��B�xB��B� B��B�qB��B��B�kB�=B�=B�=B�WB�wB��B��B�lB��B��B��B��BB��B�B��B�"B�(B��B� B�:B�&B�TBՁB�EB�KB�KB�KBچB��BޞB߾B�B�B�B�B��B��B�B�-B�B�B�%B�LB�%B�B��B��B	 B	AB	{B	�B	
�B	jB	�B	�B	�B	�B	�B	B	$B	�B	B	!B	 'B	"hB	)_B	+kB	-]B	.IB	/5B	0;B	1vB	2|B	4�B	6zB	8�B	9rB	;�B	;�B	<�B	<�B	<�B	?�B	@�B	C�B	E�B	G+B	I7B	O(B	R B	U2B	VB	W?B	XEB	YKB	YKB	[WB	\]B	]dB	^jB	_pB	_�B	b�B	f�B	i�B	k�B	l�B	m�B	n�B	n�B	q�B	tB	u%B	v�B	y	B	y�B	{0B	|6B	.B	�4B	�[B	�9B	�_B	�KB	�=B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�B	�B	�4B	�B	�B	�$B	�$B	�>B	�DB	�eB	�]B	�OB	�aB	�tB	�tB	�`B	�zB	��B	��B	��B	�lB	�rB	�rB	�xB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	ðB	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�@B	�FB	�?B	�?B	�eB	�qB	�]B	�OB	�vB	�B	�B	�tB	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�$B	�DB	�B	�B	�(B	�HB
 4B
 4B
 iB
[B
aB
MB
SB
SB
%B
YB
?B
%B
YB
YB
_B
1B
fB
fB
KB
	lB
	RB

rB

XB
^B
~B
jB
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"B
#:B
$&B
$�B
&B
&B
&B
%�B
'B
'B
'�B
($B
($B
)*B
)*B
)B
*0B
*B
*KB
+QB
,=B
-)B
-B
.IB
./B
.IB
.IB
.IB
.IB
.IB
/5B
/OB
0!B
0;B
0;B
1AB
1AB
1[B
1[B
1[B
2aB
3MB
3hB
3MB
33B
3hB
3MB
4TB
4nB
4nB
5ZB
6`B
6`B
7�B
7�B
7�B
8lB
9�B
9�B
9rB
9rB
:xB
;�B
;B
;�B
<�B
<�B
>�B
>�B
>�B
?�B
?�B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
NB
NB
O(B
OB
PB
PB
O�B
O�B
O�B
QB
QB
R:B
S&B
TB
T,B
TB
S�B
TB
S�B
TB
UB
UB
U2B
U2B
U2B
V9B
VB
VB
VB
VSB
W?B
XEB
XEB
YKB
Y1B
Y1B
ZB
Z7B
[#B
[=B
[#B
[#B
[=B
[=B
[=B
[�B
\]B
]IB
]dB
]IB
^�B
^OB
^OB
_pB
_pB
_pB
_�B
_VB
_VB
_pB
`\B
`vB
`vB
`\B
abB
a|B
abB
abB
a|B
b�B
b�B
bhB
bNB
bhB
bhB
bhB
b�B
cnB
c�B
dtB
ezB
e`B
ezB
ezB
ezB
e`B
ezB
ezB
ezB
f�B
f�B
f�B
f�B
ffB
f�B
f�B
ffB
g�B
gmB
gmB
g�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
jB
j�B
jB
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
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
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
xB
xB
y	B
x�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001190032562020011900325620200119003256202211182141362022111821413620221118214136202001200019102020012000191020200120001910  JA  ARFMdecpA19c                                                                20200109003742  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200108153748  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200108153750  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200108153751  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200108153752  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200108153752  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200108153752  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200108153752  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200108153752  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200108153752  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200108153753  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200108153753                      G�O�G�O�G�O�                JA  ARUP                                                                        20200108155414                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200108153358  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200108153330  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20200108153330  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20200108153330  CV  LONGITUDE       G�O�G�O��#��                JM  ARCAJMQC2.0                                                                 20200118153256  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200118153256  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200119151910  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124136  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                
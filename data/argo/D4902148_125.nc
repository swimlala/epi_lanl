CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-01-18T15:35:53Z creation;2018-01-18T15:35:56Z conversion to V3.1;2019-12-18T07:25:29Z update;2022-11-21T05:31:22Z update;     
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
_FillValue                 �  ]@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180118153553  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               }A   JA  I1_0397_125                     2C  Dd%�NAVIS_A                         0397                            ARGO 011514                     863 @�E�5� 1   @�E��j1�@;�KƧ��d%��rG1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A#33A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�C3Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@\)@�z�@�z�A!p�A>=qA\��A~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG�DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�9HD�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��HD�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�yHD׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�?�D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D���D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A���A���A��A��!A��!A���A���A��A�~�A�dZA�ZA�bNA�ffA�33A�(�A�JA���A��A��mA��`A��TA��;A��TA��TA��`A��`A��HA��#A���A��
A���A���A���A��RA�K�A�1'A�-A�bA���A��9A�I�A�9XA��A�
=A�%A���A��HA��!A�x�A�K�A��`A�G�A��A���A�v�A�x�A��uA�5?A�ȴA�Q�A��jA��
A�VA��DA�&�A���A��A��A��DA���A�;dA��;A���A�x�A���A�n�A���A�
=A��PA�$�A�v�A��/A���A��HA��
A��A�A�A�7LA�ȴA�^5A�VA�S�A"�A|��Az�!Ax�Aw�Aw33AvbNAut�At�At~�As�As?}Ar�/Aqp�Ao`BAnn�An(�Am�wAl�!Ak�7Ai��Ah��Ah-Ag�Ag�AfM�Ad�uAbffA`M�A^��A^bNA]�A\��A[�AZ1AV��AUhsATĜAS�AS�AS��AR �AP$�AOXAN �AKAI�PAI7LAH�AH$�AF��AF��AFjAE�FAE\)AD�yAD1AAG�A@^5A>bA<M�A;��A:Q�A9�7A9
=A8ĜA8~�A8z�A7�#A6�yA5��A4�A3ƨA3G�A1��A1"�A0Q�A/x�A/oA.��A.�A-x�A+�^A*��A*�\A*ffA)|�A(-A'�A'�A'�TA'�A'VA&�A&z�A%�A$-A#�hA"�uA"A�A"$�A!��A �/A ffA�
AO�A��A��A�`Av�A1'A�Al�AXAVA9XA�A|�AK�A&�A�RA{AhsA��A�RAz�A-A��Av�AM�AbA�wA��A��AZA�-AM�A��A�A�!A�^A
�HA
�9A
��A
v�A
=qA
�A	�A �AC�A�/Az�A1'A1A�
A�PA�RA�A�A`BA&�A�/A�+A �A��A ��@��u@��@�?}@��9@�1@�l�@�"�@��!@�E�@���@���@�@�@�p�@�A�@��;@�F@㝲@�t�@�"�@���@�{@�@�-@��@�hs@���@���@�x�@�(�@�C�@�E�@�/@��m@�C�@և+@ԣ�@��@͙�@�bN@�
=@��T@��@ț�@�b@Ǿw@�t�@�
=@Ə\@�-@�@š�@�&�@��/@���@�dZ@�5?@���@��+@�G�@�Q�@���@���@�\)@��@���@�-@��^@�O�@��@���@��u@�bN@��@�K�@��^@�r�@�C�@��@�I�@��;@�dZ@��+@�$�@��#@�?}@�l�@��@��@��;@�C�@��y@�5?@��h@�7L@�(�@��@�%@�bN@��@��P@�o@�
=@�ȴ@���@�~�@���@��/@���@��H@���@�ff@�$�@��#@�?}@��@� �@���@�\)@�K�@�
=@��@�X@���@���@��@��F@�|�@�dZ@�S�@�K�@�"�@�o@�ȴ@���@��+@�v�@�-@��T@���@�p�@�`B@��@���@���@�z�@� �@���@���@�"�@��y@�v�@�{@��^@���@���@��7@��@�`B@�V@���@�Ĝ@��j@���@��
@�@�v�@��@���@�x�@�O�@���@��u@�j@�I�@�  @�S�@��@��\@�5?@��#@�@��^@�p�@�/@��@���@�(�@��m@��w@�t�@�;d@�
=@��H@��!@���@�~�@�V@�$�@�x�@�&�@��@�I�@� �@+@~5?@|�j@|(�@{ƨ@{��@{�@{t�@{t�@{S�@{33@{@z�@z�H@z��@zJ@x�u@x �@w��@w�P@wK�@vȴ@vV@v{@u�T@u�@t1@sC�@r��@r�@qG�@p�`@p�9@p�@pA�@p  @o�@o|�@oK�@n��@n�@n��@n5?@m�-@m/@m�@l��@k��@kC�@ko@j�@j��@i��@iG�@i�@hĜ@hr�@hA�@h1'@h1'@h  @g�@g�@gl�@gK�@g+@f�y@e�@ep�@e?}@e�@d��@d�j@d1@ct�@cC�@b�H@b�@a7L@`��@`��@`�u@`r�@_�@_l�@_l�@_\)@_K�@_;d@^��@^V@^5?@]�h@\�D@\z�@\j@\j@\j@\I�@\I�@\I�@\I�@\(�@[�m@[��@[C�@Z��@Zn�@ZM�@Z=q@ZJ@Y��@Y�^@Y��@Yx�@YX@Y�@X�9@XA�@W��@W\)@W;d@Vȴ@V��@VV@V{@U�@U�T@U@U�@T�j@T��@Tz�@Tz�@TZ@T1@S�m@Sƨ@S�@St�@SdZ@St�@St�@So@R�!@R��@R=q@RJ@Q��@Q�@Q�#@Q��@Q&�@P��@P��@PQ�@O�;@Ol�@N�@N��@N��@N��@Nv�@N5?@N{@M�T@MO�@MV@L�/@L�j@L�@L�@Lz�@Lj@LI�@L(�@K��@K�@K33@J�\@I�@Ihs@I&�@I�@H��@H�9@H�@H �@G��@G|�@G;d@G�@G
=@F��@F��@FV@E�T@E@E�-@Ep�@E/@D��@D�/@D�j@D��@Dj@D1@B��@B�@A�7@AG�@A&�@@��@@bN@?
=@=@=�@<��@<z�@<Z@<I�@<�@;S�@;o@:��@:n�@:�@9��@9��@9�7@9hs@9G�@9&�@9�@8�`@8�u@8r�@7\)@5�-@5p�@5O�@5/@5�@5V@4�/@4z�@49X@41@3dZ@2��@2�\@2�\@2�\@2^5@1�#@1��@1hs@1�@0�@/��@/��@/+@.�y@.��@.v�@.ff@.$�@-�T@-p�@-?}@-/@-/@-V@,�D@,Z@,(�@,1@+�m@+ƨ@+��@+dZ@+33@+"�@+o@*�@*�H@*��@*��@*J@)��@)��@)�7@)�7@)x�@)hs@)7L@(Ĝ@( �@'�;@'�w@'|�@';d@&ȴ@&$�@%��@%`B@%?}@$��@$��@$j@$(�@#�m@#�F@#��@#�@#o@"�@"�H@"�\@"-@!�#@!�7@!G�@!�@ Ĝ@ �@   @K�@�R@ff@5?@$�@@@�@�@@�-@p�@/@��@�D@I�@(�@1@1@1@1@1@�F@�@33@�@�!@�\@~�@n�@��@x�@hs@G�@�@�@%@��@Ĝ@�9@�9@��@��@��@��@�u@�u@�u@�u@�u@�u@�u@�u@�u@�@�@r�@r�@r�@bN@bN@bN@bN@A�@1'@A�@1'@ �@�@
=@�R@�+@ff@5?@�@��@p�@V@�j@�D@I�@I�@(�@�@�F@33@��@n�@^5@M�@M�@��@G�@7L@%@��@��@�`@�`@�`@��@��@�@�@r�@r�@ �@��@K�@��@��@��@��@v�@$�@�-@�@`B@?}@/@/@V@��@�@�@��@�j@��@��@�D@j@�F@C�@
�@
��@
��@
�\@
~�@
^5@
�@	�#@	�^@	X@Ĝ@�u@ �@�w@|�@K�@K�@+@�@
=@��@�+@��@p�@O�@?}@?}@�@��@�@�/@��@�j@�j@�@��@��@9X@��@�
@�F@�@S�@�@��@��@��@��@�\@n�@-@hs@%@ Ĝ@ �9@ �9@ �u@ bN@  �?��;?��w?���?���?�5??��-?��-?��-?��h?��h?�p�?�O�?��?�(�?�ƨ?�dZ?��H?�^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A���A���A��A��!A��!A���A���A��A�~�A�dZA�ZA�bNA�ffA�33A�(�A�JA���A��A��mA��`A��TA��;A��TA��TA��`A��`A��HA��#A���A��
A���A���A���A��RA�K�A�1'A�-A�bA���A��9A�I�A�9XA��A�
=A�%A���A��HA��!A�x�A�K�A��`A�G�A��A���A�v�A�x�A��uA�5?A�ȴA�Q�A��jA��
A�VA��DA�&�A���A��A��A��DA���A�;dA��;A���A�x�A���A�n�A���A�
=A��PA�$�A�v�A��/A���A��HA��
A��A�A�A�7LA�ȴA�^5A�VA�S�A"�A|��Az�!Ax�Aw�Aw33AvbNAut�At�At~�As�As?}Ar�/Aqp�Ao`BAnn�An(�Am�wAl�!Ak�7Ai��Ah��Ah-Ag�Ag�AfM�Ad�uAbffA`M�A^��A^bNA]�A\��A[�AZ1AV��AUhsATĜAS�AS�AS��AR �AP$�AOXAN �AKAI�PAI7LAH�AH$�AF��AF��AFjAE�FAE\)AD�yAD1AAG�A@^5A>bA<M�A;��A:Q�A9�7A9
=A8ĜA8~�A8z�A7�#A6�yA5��A4�A3ƨA3G�A1��A1"�A0Q�A/x�A/oA.��A.�A-x�A+�^A*��A*�\A*ffA)|�A(-A'�A'�A'�TA'�A'VA&�A&z�A%�A$-A#�hA"�uA"A�A"$�A!��A �/A ffA�
AO�A��A��A�`Av�A1'A�Al�AXAVA9XA�A|�AK�A&�A�RA{AhsA��A�RAz�A-A��Av�AM�AbA�wA��A��AZA�-AM�A��A�A�!A�^A
�HA
�9A
��A
v�A
=qA
�A	�A �AC�A�/Az�A1'A1A�
A�PA�RA�A�A`BA&�A�/A�+A �A��A ��@��u@��@�?}@��9@�1@�l�@�"�@��!@�E�@���@���@�@�@�p�@�A�@��;@�F@㝲@�t�@�"�@���@�{@�@�-@��@�hs@���@���@�x�@�(�@�C�@�E�@�/@��m@�C�@և+@ԣ�@��@͙�@�bN@�
=@��T@��@ț�@�b@Ǿw@�t�@�
=@Ə\@�-@�@š�@�&�@��/@���@�dZ@�5?@���@��+@�G�@�Q�@���@���@�\)@��@���@�-@��^@�O�@��@���@��u@�bN@��@�K�@��^@�r�@�C�@��@�I�@��;@�dZ@��+@�$�@��#@�?}@�l�@��@��@��;@�C�@��y@�5?@��h@�7L@�(�@��@�%@�bN@��@��P@�o@�
=@�ȴ@���@�~�@���@��/@���@��H@���@�ff@�$�@��#@�?}@��@� �@���@�\)@�K�@�
=@��@�X@���@���@��@��F@�|�@�dZ@�S�@�K�@�"�@�o@�ȴ@���@��+@�v�@�-@��T@���@�p�@�`B@��@���@���@�z�@� �@���@���@�"�@��y@�v�@�{@��^@���@���@��7@��@�`B@�V@���@�Ĝ@��j@���@��
@�@�v�@��@���@�x�@�O�@���@��u@�j@�I�@�  @�S�@��@��\@�5?@��#@�@��^@�p�@�/@��@���@�(�@��m@��w@�t�@�;d@�
=@��H@��!@���@�~�@�V@�$�@�x�@�&�@��@�I�@� �@+@~5?@|�j@|(�@{ƨ@{��@{�@{t�@{t�@{S�@{33@{@z�@z�H@z��@zJ@x�u@x �@w��@w�P@wK�@vȴ@vV@v{@u�T@u�@t1@sC�@r��@r�@qG�@p�`@p�9@p�@pA�@p  @o�@o|�@oK�@n��@n�@n��@n5?@m�-@m/@m�@l��@k��@kC�@ko@j�@j��@i��@iG�@i�@hĜ@hr�@hA�@h1'@h1'@h  @g�@g�@gl�@gK�@g+@f�y@e�@ep�@e?}@e�@d��@d�j@d1@ct�@cC�@b�H@b�@a7L@`��@`��@`�u@`r�@_�@_l�@_l�@_\)@_K�@_;d@^��@^V@^5?@]�h@\�D@\z�@\j@\j@\j@\I�@\I�@\I�@\I�@\(�@[�m@[��@[C�@Z��@Zn�@ZM�@Z=q@ZJ@Y��@Y�^@Y��@Yx�@YX@Y�@X�9@XA�@W��@W\)@W;d@Vȴ@V��@VV@V{@U�@U�T@U@U�@T�j@T��@Tz�@Tz�@TZ@T1@S�m@Sƨ@S�@St�@SdZ@St�@St�@So@R�!@R��@R=q@RJ@Q��@Q�@Q�#@Q��@Q&�@P��@P��@PQ�@O�;@Ol�@N�@N��@N��@N��@Nv�@N5?@N{@M�T@MO�@MV@L�/@L�j@L�@L�@Lz�@Lj@LI�@L(�@K��@K�@K33@J�\@I�@Ihs@I&�@I�@H��@H�9@H�@H �@G��@G|�@G;d@G�@G
=@F��@F��@FV@E�T@E@E�-@Ep�@E/@D��@D�/@D�j@D��@Dj@D1@B��@B�@A�7@AG�@A&�@@��@@bN@?
=@=@=�@<��@<z�@<Z@<I�@<�@;S�@;o@:��@:n�@:�@9��@9��@9�7@9hs@9G�@9&�@9�@8�`@8�u@8r�@7\)@5�-@5p�@5O�@5/@5�@5V@4�/@4z�@49X@41@3dZ@2��@2�\@2�\@2�\@2^5@1�#@1��@1hs@1�@0�@/��@/��@/+@.�y@.��@.v�@.ff@.$�@-�T@-p�@-?}@-/@-/@-V@,�D@,Z@,(�@,1@+�m@+ƨ@+��@+dZ@+33@+"�@+o@*�@*�H@*��@*��@*J@)��@)��@)�7@)�7@)x�@)hs@)7L@(Ĝ@( �@'�;@'�w@'|�@';d@&ȴ@&$�@%��@%`B@%?}@$��@$��@$j@$(�@#�m@#�F@#��@#�@#o@"�@"�H@"�\@"-@!�#@!�7@!G�@!�@ Ĝ@ �@   @K�@�R@ff@5?@$�@@@�@�@@�-@p�@/@��@�D@I�@(�@1@1@1@1@1@�F@�@33@�@�!@�\@~�@n�@��@x�@hs@G�@�@�@%@��@Ĝ@�9@�9@��@��@��@��@�u@�u@�u@�u@�u@�u@�u@�u@�u@�@�@r�@r�@r�@bN@bN@bN@bN@A�@1'@A�@1'@ �@�@
=@�R@�+@ff@5?@�@��@p�@V@�j@�D@I�@I�@(�@�@�F@33@��@n�@^5@M�@M�@��@G�@7L@%@��@��@�`@�`@�`@��@��@�@�@r�@r�@ �@��@K�@��@��@��@��@v�@$�@�-@�@`B@?}@/@/@V@��@�@�@��@�j@��@��@�D@j@�F@C�@
�@
��@
��@
�\@
~�@
^5@
�@	�#@	�^@	X@Ĝ@�u@ �@�w@|�@K�@K�@+@�@
=@��@�+@��@p�@O�@?}@?}@�@��@�@�/@��@�j@�j@�@��@��@9X@��@�
@�F@�@S�@�@��@��@��@��@�\@n�@-@hs@%@ Ĝ@ �9@ �9@ �u@ bN@  �?��;?��w?���?���?�5??��-?��-?��-?��h?��h?�p�?�O�?��?�(�?�ƨ?�dZ?��H?�^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BJB��B��B��B�B�B�B�B��B��B��B��B�B�B�B�yB�;B��BŢB��B�RB��B�JB�By�Bo�BaHBL�BA�B2-B(�B�BJB�sB��B�wB�LB�!B�B��B��B�bBr�BB�B�BB
�B
�`B
��B
��B
�!B
��B
��B
�7B
�B
�'B
��B
�hB
�%B
w�B
k�B
`BB
XB
R�B
M�B
H�B
D�B
B�B
=qB
7LB
2-B
&�B
�B
�B
�B
�B
bB
	7B	��B	��B	�B	�B	�B	�ZB	�B	ƨB	�XB	��B	��B	��B	�uB	�7B	x�B	]/B	N�B	K�B	J�B	O�B	L�B	8RB	 �B	�B	1B�B�NB�BB�5B�B��B�B�)B�;B�;B�fB�`B��BÖB�'B��B��B��B�oB�\B�oB��B��B��B��B��B�uB�VB�DB�+B�B�B�B�B�B�B�B�B�B�B�B�B�B� B� B� B� B� B~�B}�B{�Bz�Bz�By�By�By�B{�Bz�Bx�Bw�Bw�Bx�Bz�B� B�B�B�B�B�B�B� B~�B~�B~�B}�B|�Bz�Bw�Bv�Bt�Bq�Bk�BgmBffBffBe`BdZBcTBbNB`BB^5B[#BZBXBW
BT�BR�BR�BR�BQ�BP�BO�BM�BK�BJ�BJ�BK�BK�BK�BJ�BI�BG�BD�BB�BA�BA�B@�B?}B>wB<jB8RB7LB6FB5?B49B49B49B33B33B2-B1'B-B+B)�B+B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B(�B(�B'�B&�B&�B&�B%�B%�B%�B&�B&�B%�B$�B&�B(�B(�B)�B+B,B,B-B-B.B.B/B/B0!B0!B1'B1'B33B33B5?B:^B<jB>wBA�BC�BC�BC�BD�BD�BE�BF�BG�BG�BH�BH�BH�BH�BI�BL�BN�BP�BS�BXBYBZB\)B\)B\)B]/B`BBcTBgmBhsBjBk�Bn�Bp�Bp�Bt�Bx�B� B�B�B�B�1B�1B�1B�7B�7B�DB�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�9B�LB�RB�XB�^B�^B�^B�dB�dB�qB�wB�wB�wB��BBÖBĜBŢBƨBȴBɺB��B��B��B��B��B�B�B�#B�5B�5B�;B�;B�;B�;B�HB�ZB�ZB�TB�ZB�yB�B�B��B��B��B��B��B	B	B	B	B	
=B	PB	bB	oB	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	&�B	(�B	+B	,B	.B	/B	/B	1'B	2-B	33B	8RB	;dB	@�B	B�B	C�B	G�B	K�B	R�B	VB	XB	XB	XB	YB	YB	YB	ZB	ZB	[#B	[#B	[#B	]/B	bNB	dZB	e`B	e`B	ffB	hsB	iyB	jB	k�B	k�B	q�B	s�B	u�B	w�B	z�B	|�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�%B	�+B	�7B	�JB	�JB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�?B	�?B	�LB	�XB	�qB	�qB	�wB	�wB	�wB	�}B	��B	��B	B	B	B	ÖB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�NB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�mB	�mB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
PB
\B
oB
uB
uB
uB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
"�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
,B
-B
-B
.B
.B
/B
/B
/B
/B
/B
0!B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
5?B
6FB
6FB
5?B
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
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
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
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
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
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
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
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
s�B
s�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�B�B�B�B�B�B�B�B�B�B�B�BB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BB�B"B�B�rB�8B��B�hB��B��B��B��B��B�+B�TB�-B�IB�B�vB��B�?B��B�^B��B�jB�9B{Bq'Bc:BN<BC-B3MB*eB"hB}B�B��B�}B�B��B��B�sB��B��Bx�BG�B�B�B
��B
��B
�NB
��B
�B
��B
�=B
�RB
�OB
��B
�'B
��B
�B
zB
mwB
a�B
Y1B
S�B
N�B
IlB
E9B
CGB
>wB
88B
4B
)*B
 �B
)B
kB
B
B
B	�(B	�tB	�MB	��B	�B	��B	��B	�7B	�0B	��B	�B	�B	�B	��B	|B	_B	O�B	L�B	KB	P�B	N�B	:�B	"B	�B	B��B��B��B�VB�kB՛B֡B�B��B�BB�$B�sB��B�?B�MB�B�HB��B�&B��B��B�B��B�B�7B��B��B�vB��B��B�YB�B��B��B��B�gB�B�B��B��B�uB�[B�UB�OB�OB��B��B��B�B}B}�B|B|Bz^BzDBz�B|�B{�By�Bx�Bx�Bz*B|6B��B��B��B��B�{B��B�B��BcBcBcB~�B}�B{�Bx�Bw2Bu�BtTBl�Bh$Bf�Bf�Be�Bd�BdtBc:Ba|B_�B[�B[	BX�BX_BVBS@BS@BS@BRTBQhBP�BO�BMBK^BKxBL0BL0BL0BK�BJ�BH�BESBC-BA�BBBA;B@iB?�B>BB;JB8�B6�B5�B4�B4�B4�B3�B3�B3B3B1�B.B,B+�B*eB*0B*KB*KB*KB*B*B*eB*KB*0B)yB)�B)_B'�B'�B'�B&�B&�B&�B'�B'�B'�B'mB)B)�B)�B*�B+�B,�B,qB-wB-wB.}B.�B/�B/�B0oB0�B1�B1�B3�B4TB6�B;JB=VB?HBB'BC�BC�BC�BEBE9BF%BGBG�BHBIBIBI7BI�BJ�BM�BO�BRBT�BX�BY�BZ�B\�B\�B\�B^�Ba|Bd@Bh
BiBj�Bl"BoBqABq�Bu�Bz*B��B��B�gB�mB�KB��B��B��B��B�B�HB�B��B��B�	B�	B�)B�;B�bB�ZB�8B�>B�sB��B��B�vB��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�B�"B�bB�,B�mB�yBیB�OB�jB�pB�pBߊBߤB�|B�tB�B�B�B�0B�B�B�%B�$B�*B�JB�<B	UB	[B	uB	�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	)B	;B	$&B	%B	'8B	)*B	+6B	,=B	.IB	/5B	/iB	1vB	2�B	3�B	8�B	;�B	@�B	B�B	D3B	HB	LdB	S@B	V9B	X+B	X+B	X+B	Y1B	Y1B	Y1B	ZQB	ZQB	[WB	[qB	[�B	]�B	b�B	dtB	ezB	e�B	f�B	h�B	i�B	j�B	k�B	l"B	rB	tB	v+B	xB	{B	}B	B	B	�B	�;B	�;B	�'B	�GB	�MB	�9B	�tB	�_B	�lB	�~B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	��B	�B	�RB	�KB	�=B	�)B	�IB	�cB	�iB	�aB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�.B	�B	�&B	�2B	�9B	�9B	�?B	�+B	�+B	�+B	�EB	�eB	�kB	�WB	�dB	�OB	�jB	�pB	�\B	�|B	�bB	�hB	�B	�B	�tB	�zB	�zB	�`B	�B	�B	�B	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	�B	�B	�VB	�.B
UB
-B
GB
-B
MB
3B
gB
?B
YB
?B
EB
KB
	RB
	lB

rB
xB
^B
^B
xB
^B
xB
xB
^B
~B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
 �B
!HB
#TB
'B
($B
(
B
(
B
($B
(
B
)*B
)*B
*B
*0B
+6B
,=B
-)B
-)B
.IB
.cB
/5B
/OB
/iB
/OB
/OB
0;B
1vB
2GB
2GB
2aB
3MB
3hB
4TB
4�B
5tB
6FB
6`B
5tB
6�B
7�B
7�B
7�B
7�B
7fB
8lB
8�B
8lB
9rB
9rB
9�B
9rB
9rB
9�B
9�B
:�B
;B
;B
;dB
;B
;�B
;B
;�B
<�B
=�B
>�B
>�B
>�B
?�B
?�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
IB
IB
I�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
MB
MB
L�B
MB
M�B
M�B
OB
OB
OB
N�B
N�B
N�B
N�B
OB
O�B
O�B
PB
QB
QB
QB
Q B
Q4B
R B
SB
SB
S&B
SB
SB
S&B
TB
S�B
TB
S�B
TB
TB
TB
S�B
TB
TB
S�B
S�B
TB
TB
TB
S�B
S�B
TB
T�B
UB
UB
T�B
UB
UB
UB
UB
UB
T�B
UB
U2B
U2B
V9B
W?B
X+B
XEB
X+B
X+B
X+B
Y1B
YKB
Z7B
ZQB
ZQB
[#B
[WB
[=B
[qB
[qB
\]B
]dB
]/B
]dB
]IB
]dB
^jB
_;B
_pB
_VB
_;B
_;B
_VB
_;B
_VB
_pB
_pB
`\B
`\B
`\B
`\B
`�B
abB
abB
b�B
bhB
bhB
b�B
b�B
cnB
c�B
dtB
dtB
dZB
dtB
d�B
dtB
ezB
e`B
e�B
e`B
e�B
e`B
ezB
e�B
e�B
f�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
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
s�B
tB
uB
u�B
v�B
v�B
v�B
v�B
v�B
v�B
xB
xB
xB
y	B
y	B
y	B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
|B
|B
|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801300031582018013000315820180130003158202211182133202022111821332020221118213320201804031938592018040319385920180403193859  JA  ARFMdecpA19c                                                                20180119003513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180118153553  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180118153554  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180118153555  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180118153555  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180118153555  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180118153555  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180118153555  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180118153556  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180118153556                      G�O�G�O�G�O�                JA  ARUP                                                                        20180118155608                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180118153250  CV  JULD            G�O�G�O�F�-                JM  ARCAJMQC2.0                                                                 20180129153158  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180129153158  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103859  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171535                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123320  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                
CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-21T09:00:51Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         C   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    9   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    9    HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    9$   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    9(   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    98   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    9H   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    9X   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  9`   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  9�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  @  9�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        :    	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    :$   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    :(   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     :,   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    :L   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    :P   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     :T   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     :t   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     :�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_NB_SAMPLE_CTD_QC               	long_name         ,Global quality flag of NB_SAMPLE_CTD profile   conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        h  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kd   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  O@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ^�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  b�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  q�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʈ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �d   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  ��   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20230421090051  20230421090051  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @�%V�z�t1   @�%W/hWB@+�dZ��d���1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A       @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B���B�  B�  B�  B�ffBÙ�B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��fD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��G@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B���B�ǮB��{B��{B�ǮB�ǮB�ǮB�.B�aHBǔ{B�ǮB�ǮBӔ{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmr�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA��A��#Aڥ�A�VA���A�9XA؟�A؍PA؅A�|�A�r�A�XA�A�A�"�A�A�Aו�A�l�A�VA�G�A�+A��A֍PA�^5A��TA�l�A�
=A��
A�x�A�VA�%A�ȴA���A�n�A� �A�VA�$�A��TA�S�A�1A�{A���A�t�A�G�A�p�A�A�=qA�XA�z�A���A���A�%A��A��uA�ffA�M�A��#A��hA���A�A���A���A�hsA�VA�VA�\)A��mA�I�A��A��hA��A�ƨA�7LA�A���A��
A���A��wA��A���A�A�A~ffAvffAlbNAi/Ad��A_�A\jAV�AT�RAQ`BANn�AJ�HAH��AE��ADZAC�AB�jAA�A@=qA?VA>�A;�;A;p�A:ffA:�`A:�/A9��A8ĜA6�!A3p�A0�`A0{A/oA-�A,��A,�uA,A�A+�mA+t�A)�A(�yA'��A'l�A&��A&ffA%��A$�9A#��A#XA#VA"��A!��A ĜA 5?A��AC�A�AVAr�Ax�A��A  A`BA�A�A�!AQ�A�TA`BA/A+A;dA7LA��A�A�!A�uAI�A�Ax�A�HA�;AA��AjA(�A�;A�A1A �A�mA
=AE�A�+A�`A��A�A7LA�A��A�yA��A�Az�A��AK�A��A$�A��A��A�hA�AS�AVA�DA(�A��A�wA�^A�wA�-AG�A
�/A
��A
�\A
A�A	�A	�-A	p�A	XA	+A��A�jA9XAp�A33AȴA�\AZA5?A�A�A+AoA��AȴA�+A �A�
AG�AVA�A�A�A1'AA�AoA �DA  �@��;@�;d@���@�V@�O�@�5?@��j@�z�@�b@��m@��w@���@�S�@�"�@���@���@�x�@��9@��
@�@�dZ@�l�@�l�@�"�@��@��@���@�G�@�V@�1'@�t�@@�hs@�u@�C�@�o@��y@ꗍ@�5?@�?}@��@��@�I�@�  @�33@���@���@�h@�?}@��@���@�1'@�o@�\@��T@ᙚ@�7@�O�@���@��@�j@�9X@ߝ�@�"�@��@ް!@�5?@ݲ-@�G�@�Ĝ@�bN@۶F@�ȴ@�-@��#@�7L@�j@׮@��@���@�~�@���@Ձ@�&�@�Ĝ@�z�@ӍP@�"�@��@��@�~�@��@Ѳ-@�X@���@�r�@�(�@϶F@��@���@���@ΰ!@�v�@�V@�$�@Ͳ-@�`B@�%@�Ĝ@���@���@�z�@�9X@�ƨ@ˍP@�S�@�ȴ@�~�@�$�@��#@�x�@��@��/@���@���@ȴ9@ȋD@�  @�l�@�=q@ř�@�X@��@ģ�@Õ�@�33@�ȴ@���@�7L@��@��@��@�j@��@���@��@�5?@�J@���@�/@���@��u@��@��m@��F@��P@���@��T@�O�@�%@���@�l�@�^5@���@��7@�G�@���@���@���@�Ĝ@� �@��F@�|�@��@�?}@���@�Q�@�(�@�b@�ƨ@�+@���@���@�^5@�^5@�E�@�-@�{@��-@�/@���@��@�9X@���@�"�@��\@�V@�V@��@�X@��@���@�j@� �@���@��P@�@��R@�ff@�@��^@���@�G�@���@��@�1'@��;@�dZ@���@��^@��@��@�j@�bN@�bN@�1'@��@��@�;d@���@��y@���@�~�@�^5@�$�@�J@��@��@��9@��u@�z�@�bN@��@��F@��@�\)@�33@��y@��\@�=q@���@�X@��/@�Ĝ@��u@�1'@�ƨ@�;d@�5?@���@��@���@�Q�@� �@��
@�|�@�@���@��+@�$�@���@��7@�?}@���@��9@��@�9X@�b@��
@�|�@�@���@�@���@�&�@��u@�r�@�I�@�b@�1@�  @���@��w@��P@�\)@�+@���@��+@�v�@�5?@�@��@�@�G�@���@�bN@��@�  @��;@��@�l�@�C�@�o@��y@���@�M�@���@�x�@��@�V@��9@� �@�ƨ@���@�S�@��@�ȴ@���@��\@�^5@�5?@��T@���@��@�`B@�7L@���@��j@�Q�@��@\)@~�@~��@~�+@~v�@~{@}`B@|�@{��@{dZ@{"�@y��@y�@xr�@xb@w��@v��@vv�@v@u`B@uV@t�D@t9X@s��@s�
@s�@so@r�!@r~�@r=q@r�@q�@q�^@qX@p��@p �@o��@o+@nȴ@n��@n�+@nv�@n{@lz�@k�m@k�@j�@j�!@j��@j~�@jn�@j^5@jM�@j=q@j�@i��@i��@iG�@hr�@g�P@g�@f�R@fv�@fE�@fE�@e�-@d��@c�m@bn�@a�@a�^@a�^@ax�@`�9@_�@_K�@_�@^��@]��@]��@]?}@\�/@\Z@[ƨ@[�@Z�H@Z��@ZJ@Yhs@X�`@Xr�@XbN@Xb@W�P@W+@W�@V��@V�y@V�R@VV@V@U��@U�-@U�h@U�@Up�@U?}@T�@T�@T��@Tz�@T(�@T1@T1@S�m@St�@S@R�\@R=q@QG�@P�u@P1'@O�;@O�@O|�@OK�@N��@M�T@M/@L�D@LI�@L1@K�m@K��@KS�@K@J�H@J~�@Jn�@Jn�@J^5@J^5@JM�@I�@I��@I��@IX@I%@HĜ@H��@Hr�@H  @G�w@G
=@Fȴ@F��@F��@FV@F@E��@E�-@E`B@EV@D�j@Dj@DZ@DI�@D1@C�m@Cƨ@C�@CS�@B�@B�H@B��@B�!@B��@B~�@A��@Ax�@Ahs@A�@A%@A%@@��@@�9@@r�@@1'@?�@?K�@>�@>ff@>5?@>$�@>{@=�@=�-@<Z@;��@;�m@;ƨ@;�F@;t�@;@:�@9X@8�`@8��@8bN@7�;@6ff@5�@5�h@5�h@5�@5p�@5O�@5�@4�@4�D@3��@3�
@3�m@3�m@3��@3�m@3�F@3t�@3C�@3"�@2�H@2��@2~�@2=q@2J@2J@1�^@1G�@0��@0�`@0�`@0�`@0��@0�u@/��@/�P@.��@.�R@.��@.��@.V@-�T@-p�@-�@-V@,�@,��@,Z@,�@+��@+�
@+�F@+�@+33@*�@*-@)�@)�#@)�#@)�#@)�#@)��@*J@*-@)��@)�7@)&�@(��@(��@(r�@'�;@'�@'�P@'l�@'l�@';d@&�y@&ȴ@&�+@&E�@&5?@&{@%@%p�@%�@$��@$�@$�@$I�@$1@#ƨ@#dZ@#o@"��@"~�@!�#@!X@ ��@ r�@ A�@ b@�@�P@l�@
=@�R@��@�+@ff@E�@{@@p�@?}@V@�@�/@��@�j@z�@9X@1@�F@dZ@33@o@��@n�@n�@^5@M�@=q@J@��@G�@��@�`@�9@�u@�@r�@1'@�;@�w@�w@�w@�@��@l�@+@�y@ȴ@�+@ff@ff@V@V@5?@5?@$�@��@��@p�@?}@�@��@�j@z�@ƨ@S�@"�@@@�@�H@�H@�H@��@��@~�@�\@~�@^5@=q@-@J@�^@��@��@�7@hs@X@&�@7L11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��yA��A��#Aڥ�A�VA���A�9XA؟�A؍PA؅A�|�A�r�A�XA�A�A�"�A�A�Aו�A�l�A�VA�G�A�+A��A֍PA�^5A��TA�l�A�
=A��
A�x�A�VA�%A�ȴA���A�n�A� �A�VA�$�A��TA�S�A�1A�{A���A�t�A�G�A�p�A�A�=qA�XA�z�A���A���A�%A��A��uA�ffA�M�A��#A��hA���A�A���A���A�hsA�VA�VA�\)A��mA�I�A��A��hA��A�ƨA�7LA�A���A��
A���A��wA��A���A�A�A~ffAvffAlbNAi/Ad��A_�A\jAV�AT�RAQ`BANn�AJ�HAH��AE��ADZAC�AB�jAA�A@=qA?VA>�A;�;A;p�A:ffA:�`A:�/A9��A8ĜA6�!A3p�A0�`A0{A/oA-�A,��A,�uA,A�A+�mA+t�A)�A(�yA'��A'l�A&��A&ffA%��A$�9A#��A#XA#VA"��A!��A ĜA 5?A��AC�A�AVAr�Ax�A��A  A`BA�A�A�!AQ�A�TA`BA/A+A;dA7LA��A�A�!A�uAI�A�Ax�A�HA�;AA��AjA(�A�;A�A1A �A�mA
=AE�A�+A�`A��A�A7LA�A��A�yA��A�Az�A��AK�A��A$�A��A��A�hA�AS�AVA�DA(�A��A�wA�^A�wA�-AG�A
�/A
��A
�\A
A�A	�A	�-A	p�A	XA	+A��A�jA9XAp�A33AȴA�\AZA5?A�A�A+AoA��AȴA�+A �A�
AG�AVA�A�A�A1'AA�AoA �DA  �@��;@�;d@���@�V@�O�@�5?@��j@�z�@�b@��m@��w@���@�S�@�"�@���@���@�x�@��9@��
@�@�dZ@�l�@�l�@�"�@��@��@���@�G�@�V@�1'@�t�@@�hs@�u@�C�@�o@��y@ꗍ@�5?@�?}@��@��@�I�@�  @�33@���@���@�h@�?}@��@���@�1'@�o@�\@��T@ᙚ@�7@�O�@���@��@�j@�9X@ߝ�@�"�@��@ް!@�5?@ݲ-@�G�@�Ĝ@�bN@۶F@�ȴ@�-@��#@�7L@�j@׮@��@���@�~�@���@Ձ@�&�@�Ĝ@�z�@ӍP@�"�@��@��@�~�@��@Ѳ-@�X@���@�r�@�(�@϶F@��@���@���@ΰ!@�v�@�V@�$�@Ͳ-@�`B@�%@�Ĝ@���@���@�z�@�9X@�ƨ@ˍP@�S�@�ȴ@�~�@�$�@��#@�x�@��@��/@���@���@ȴ9@ȋD@�  @�l�@�=q@ř�@�X@��@ģ�@Õ�@�33@�ȴ@���@�7L@��@��@��@�j@��@���@��@�5?@�J@���@�/@���@��u@��@��m@��F@��P@���@��T@�O�@�%@���@�l�@�^5@���@��7@�G�@���@���@���@�Ĝ@� �@��F@�|�@��@�?}@���@�Q�@�(�@�b@�ƨ@�+@���@���@�^5@�^5@�E�@�-@�{@��-@�/@���@��@�9X@���@�"�@��\@�V@�V@��@�X@��@���@�j@� �@���@��P@�@��R@�ff@�@��^@���@�G�@���@��@�1'@��;@�dZ@���@��^@��@��@�j@�bN@�bN@�1'@��@��@�;d@���@��y@���@�~�@�^5@�$�@�J@��@��@��9@��u@�z�@�bN@��@��F@��@�\)@�33@��y@��\@�=q@���@�X@��/@�Ĝ@��u@�1'@�ƨ@�;d@�5?@���@��@���@�Q�@� �@��
@�|�@�@���@��+@�$�@���@��7@�?}@���@��9@��@�9X@�b@��
@�|�@�@���@�@���@�&�@��u@�r�@�I�@�b@�1@�  @���@��w@��P@�\)@�+@���@��+@�v�@�5?@�@��@�@�G�@���@�bN@��@�  @��;@��@�l�@�C�@�o@��y@���@�M�@���@�x�@��@�V@��9@� �@�ƨ@���@�S�@��@�ȴ@���@��\@�^5@�5?@��T@���@��@�`B@�7L@���@��j@�Q�@��@\)@~�@~��@~�+@~v�@~{@}`B@|�@{��@{dZ@{"�@y��@y�@xr�@xb@w��@v��@vv�@v@u`B@uV@t�D@t9X@s��@s�
@s�@so@r�!@r~�@r=q@r�@q�@q�^@qX@p��@p �@o��@o+@nȴ@n��@n�+@nv�@n{@lz�@k�m@k�@j�@j�!@j��@j~�@jn�@j^5@jM�@j=q@j�@i��@i��@iG�@hr�@g�P@g�@f�R@fv�@fE�@fE�@e�-@d��@c�m@bn�@a�@a�^@a�^@ax�@`�9@_�@_K�@_�@^��@]��@]��@]?}@\�/@\Z@[ƨ@[�@Z�H@Z��@ZJ@Yhs@X�`@Xr�@XbN@Xb@W�P@W+@W�@V��@V�y@V�R@VV@V@U��@U�-@U�h@U�@Up�@U?}@T�@T�@T��@Tz�@T(�@T1@T1@S�m@St�@S@R�\@R=q@QG�@P�u@P1'@O�;@O�@O|�@OK�@N��@M�T@M/@L�D@LI�@L1@K�m@K��@KS�@K@J�H@J~�@Jn�@Jn�@J^5@J^5@JM�@I�@I��@I��@IX@I%@HĜ@H��@Hr�@H  @G�w@G
=@Fȴ@F��@F��@FV@F@E��@E�-@E`B@EV@D�j@Dj@DZ@DI�@D1@C�m@Cƨ@C�@CS�@B�@B�H@B��@B�!@B��@B~�@A��@Ax�@Ahs@A�@A%@A%@@��@@�9@@r�@@1'@?�@?K�@>�@>ff@>5?@>$�@>{@=�@=�-@<Z@;��@;�m@;ƨ@;�F@;t�@;@:�@9X@8�`@8��@8bN@7�;@6ff@5�@5�h@5�h@5�@5p�@5O�@5�@4�@4�D@3��@3�
@3�m@3�m@3��@3�m@3�F@3t�@3C�@3"�@2�H@2��@2~�@2=q@2J@2J@1�^@1G�@0��@0�`@0�`@0�`@0��@0�u@/��@/�P@.��@.�R@.��@.��@.V@-�T@-p�@-�@-V@,�@,��@,Z@,�@+��@+�
@+�F@+�@+33@*�@*-@)�@)�#@)�#@)�#@)�#@)��@*J@*-@)��@)�7@)&�@(��@(��@(r�@'�;@'�@'�P@'l�@'l�@';d@&�y@&ȴ@&�+@&E�@&5?@&{@%@%p�@%�@$��@$�@$�@$I�@$1@#ƨ@#dZ@#o@"��@"~�@!�#@!X@ ��@ r�@ A�@ b@�@�P@l�@
=@�R@��@�+@ff@E�@{@@p�@?}@V@�@�/@��@�j@z�@9X@1@�F@dZ@33@o@��@n�@n�@^5@M�@=q@J@��@G�@��@�`@�9@�u@�@r�@1'@�;@�w@�w@�w@�@��@l�@+@�y@ȴ@�+@ff@ff@V@V@5?@5?@$�@��@��@p�@?}@�@��@�j@z�@ƨ@S�@"�@@@�@�H@�H@�H@��@��@~�@�\@~�@^5@=q@-@J@�^@��@��@�7@hs@X@&�@7L11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
\B
JB
1B
B
B	��B
B
JB
JB
DB

=B
1B
%B
B
B
  B
B	��B	��B	��B	��B	��B	��B
%B
1B
�B
)�B
33B
33B
2-B
+B
"�B
bB	�B	�;B	��B	�B
B
DB
B
B
oB
�B
@�B
M�B
��B
��B
�BB;dB�dB��B��B�B�BBB��B��B�HB��B�B�}B��BYBYBD�B(�BB
��B
ŢB
�PB
hsB
iyB
N�B
/B
,B
B
�B
�B
uB	��B	��B	��B	ffB	|�B	jB	G�B	>wB	5?B	8RB	5?B	49B	C�B	M�B	L�B	aHB	o�B	jB	m�B	ffB	n�B	s�B	n�B	�{B	�B	ĜB	�
B	�B	��B	�fB	��B	ŢB	�
B	��B	�wB	ÖB	ɺB	ɺB	ȴB	ȴB	ĜB	ȴB	��B	ɺB	ĜB	��B	��B	��B	��B	�B	�B	�B	�fB	�B	��B	��B
B
+B
DB
JB
	7B
VB
�B
�B
 �B
"�B
!�B
�B
�B
!�B
'�B
+B
0!B
/B
-B
.B
.B
-B
+B
(�B
(�B
&�B
%�B
+B
5?B
:^B
@�B
C�B
G�B
P�B
P�B
Q�B
F�B
H�B
XB
aHB
cTB
e`B
hsB
iyB
gmB
dZB
\)B
N�B
F�B
H�B
D�B
E�B
G�B
K�B
O�B
Q�B
P�B
O�B
M�B
K�B
L�B
N�B
XB
ZB
[#B
ZB
W
B
VB
[#B
XB
T�B
VB
T�B
VB
VB
T�B
P�B
M�B
H�B
D�B
J�B
I�B
J�B
K�B
J�B
G�B
E�B
E�B
I�B
H�B
E�B
F�B
C�B
E�B
A�B
F�B
G�B
E�B
B�B
A�B
A�B
=qB
:^B
;dB
:^B
;dB
9XB
9XB
5?B
.B
$�B
(�B
33B
2-B
49B
33B
2-B
1'B
0!B
-B
)�B
)�B
)�B
(�B
-B
1'B
33B
49B
2-B
1'B
/B
0!B
/B
.B
)�B
&�B
$�B
"�B
!�B
"�B
'�B
'�B
&�B
%�B
"�B
)�B
)�B
+B
+B
&�B
'�B
&�B
+B
)�B
+B
)�B
&�B
%�B
'�B
(�B
+B
,B
+B
+B
(�B
+B
)�B
'�B
(�B
(�B
(�B
%�B
&�B
%�B
$�B
$�B
"�B
 �B
"�B
"�B
!�B
�B
�B
!�B
"�B
!�B
�B
!�B
 �B
 �B
�B
�B
 �B
!�B
!�B
 �B
�B
!�B
 �B
�B
�B
�B
�B
�B
!�B
"�B
"�B
 �B
!�B
 �B
�B
 �B
!�B
!�B
#�B
"�B
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
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
$�B
$�B
#�B
#�B
!�B
 �B
!�B
"�B
!�B
 �B
"�B
#�B
&�B
'�B
$�B
!�B
%�B
&�B
'�B
&�B
&�B
$�B
#�B
&�B
%�B
$�B
%�B
%�B
#�B
"�B
"�B
#�B
#�B
!�B
 �B
�B
"�B
$�B
&�B
'�B
'�B
%�B
$�B
$�B
%�B
&�B
'�B
'�B
%�B
'�B
&�B
&�B
$�B
$�B
)�B
)�B
+B
)�B
(�B
'�B
)�B
+B
)�B
)�B
(�B
)�B
)�B
(�B
)�B
-B
-B
+B
)�B
)�B
'�B
-B
.B
-B
-B
0!B
0!B
/B
/B
2-B
1'B
0!B
1'B
5?B
5?B
49B
7LB
6FB
6FB
6FB
5?B
49B
33B
49B
33B
6FB
5?B
5?B
9XB
9XB
9XB
;dB
;dB
;dB
9XB
9XB
:^B
9XB
8RB
<jB
;dB
;dB
;dB
<jB
:^B
9XB
:^B
;dB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
=qB
=qB
>wB
=qB
?}B
A�B
@�B
>wB
A�B
C�B
C�B
C�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
I�B
I�B
I�B
H�B
G�B
G�B
I�B
I�B
J�B
L�B
L�B
L�B
K�B
I�B
J�B
J�B
M�B
L�B
J�B
L�B
N�B
P�B
O�B
O�B
P�B
P�B
P�B
Q�B
P�B
Q�B
R�B
R�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
R�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
T�B
T�B
S�B
Q�B
O�B
R�B
T�B
T�B
W
B
XB
XB
XB
XB
XB
XB
XB
W
B
VB
VB
T�B
W
B
XB
YB
ZB
ZB
ZB
XB
VB
XB
VB
[#B
]/B
]/B
\)B
ZB
[#B
\)B
]/B
]/B
\)B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
^5B
_;B
_;B
`BB
bNB
aHB
aHB
aHB
bNB
cTB
bNB
bNB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
dZB
dZB
cTB
cTB
dZB
dZB
cTB
bNB
bNB
bNB
bNB
aHB
bNB
dZB
e`B
e`B
e`B
e`B
cTB
dZB
e`B
e`B
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
iyB
hsB
iyB
iyB
iyB
hsB
iyB
jB
iyB
hsB
iyB
hsB
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
m�B
l�B
n�B
o�B
n�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
m�B
n�B
o�B
o�B
p�B
o�B
o�B
n�B
l�B
p�B
r�B
q�B
q�B
p�B
o�B
n�B
o�B
q�B
r�B
q�B
p�B
n�B
o�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
u�B
v�B
v�B
v�B
v�B
u�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
u�B
v�B
u�B
t�B
u�B
w�B
w�B
v�B
v�B
u�B
s�B
u�B
t�B
u�B
w�B
v�B
v�B
u�B
v�B
w�B
x�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
w�B
w�B
w�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
|�B
z�B
{�B
z�B
{�B
z�B
y�B
y�B
{�B
|�B
}�B
}�B
|�B
|�B
}�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
� B
� B
~�B
~�B
~�B
~�B
� B
� B
� B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�+B
�+B
�%B
�%B
�%B
�%B
�+B
�+B
�1B
�+B
�1B
�7B
�7B
�7B
�1B
�1B
�+B
�1B
�7B
�=B
�7B
�7B
�=B
�=B
�7B
�7B
�=B
�=B
�=B
�=B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�DB
�=B
�DB
�=B
�7B
�=B
�DB
�DB
�DB
�PB
�JB
�JB
�DB
�DB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�VB
�bB
�bB
�bB
�bB
�bB
�hB
�oB
�o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
\B
JB
1B
B
B	��B
B
JB
JB
DB

=B
1B
%B
B
B
  B
B	��B	��B	��B	��B	��B	��B
%B
1B
�B
)�B
33B
33B
2-B
+B
"�B
bB	�B	�;B	��B	�B
B
DB
B
B
oB
�B
@�B
M�B
��B
��B
�BB;dB�dB��B��B�B�BBB��B��B�HB��B�B�}B��BYBYBD�B(�BB
��B
ŢB
�PB
hsB
iyB
N�B
/B
,B
B
�B
�B
uB	��B	��B	��B	ffB	|�B	jB	G�B	>wB	5?B	8RB	5?B	49B	C�B	M�B	L�B	aHB	o�B	jB	m�B	ffB	n�B	s�B	n�B	�{B	�B	ĜB	�
B	�B	��B	�fB	��B	ŢB	�
B	��B	�wB	ÖB	ɺB	ɺB	ȴB	ȴB	ĜB	ȴB	��B	ɺB	ĜB	��B	��B	��B	��B	�B	�B	�B	�fB	�B	��B	��B
B
+B
DB
JB
	7B
VB
�B
�B
 �B
"�B
!�B
�B
�B
!�B
'�B
+B
0!B
/B
-B
.B
.B
-B
+B
(�B
(�B
&�B
%�B
+B
5?B
:^B
@�B
C�B
G�B
P�B
P�B
Q�B
F�B
H�B
XB
aHB
cTB
e`B
hsB
iyB
gmB
dZB
\)B
N�B
F�B
H�B
D�B
E�B
G�B
K�B
O�B
Q�B
P�B
O�B
M�B
K�B
L�B
N�B
XB
ZB
[#B
ZB
W
B
VB
[#B
XB
T�B
VB
T�B
VB
VB
T�B
P�B
M�B
H�B
D�B
J�B
I�B
J�B
K�B
J�B
G�B
E�B
E�B
I�B
H�B
E�B
F�B
C�B
E�B
A�B
F�B
G�B
E�B
B�B
A�B
A�B
=qB
:^B
;dB
:^B
;dB
9XB
9XB
5?B
.B
$�B
(�B
33B
2-B
49B
33B
2-B
1'B
0!B
-B
)�B
)�B
)�B
(�B
-B
1'B
33B
49B
2-B
1'B
/B
0!B
/B
.B
)�B
&�B
$�B
"�B
!�B
"�B
'�B
'�B
&�B
%�B
"�B
)�B
)�B
+B
+B
&�B
'�B
&�B
+B
)�B
+B
)�B
&�B
%�B
'�B
(�B
+B
,B
+B
+B
(�B
+B
)�B
'�B
(�B
(�B
(�B
%�B
&�B
%�B
$�B
$�B
"�B
 �B
"�B
"�B
!�B
�B
�B
!�B
"�B
!�B
�B
!�B
 �B
 �B
�B
�B
 �B
!�B
!�B
 �B
�B
!�B
 �B
�B
�B
�B
�B
�B
!�B
"�B
"�B
 �B
!�B
 �B
�B
 �B
!�B
!�B
#�B
"�B
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
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
$�B
$�B
#�B
#�B
!�B
 �B
!�B
"�B
!�B
 �B
"�B
#�B
&�B
'�B
$�B
!�B
%�B
&�B
'�B
&�B
&�B
$�B
#�B
&�B
%�B
$�B
%�B
%�B
#�B
"�B
"�B
#�B
#�B
!�B
 �B
�B
"�B
$�B
&�B
'�B
'�B
%�B
$�B
$�B
%�B
&�B
'�B
'�B
%�B
'�B
&�B
&�B
$�B
$�B
)�B
)�B
+B
)�B
(�B
'�B
)�B
+B
)�B
)�B
(�B
)�B
)�B
(�B
)�B
-B
-B
+B
)�B
)�B
'�B
-B
.B
-B
-B
0!B
0!B
/B
/B
2-B
1'B
0!B
1'B
5?B
5?B
49B
7LB
6FB
6FB
6FB
5?B
49B
33B
49B
33B
6FB
5?B
5?B
9XB
9XB
9XB
;dB
;dB
;dB
9XB
9XB
:^B
9XB
8RB
<jB
;dB
;dB
;dB
<jB
:^B
9XB
:^B
;dB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
=qB
=qB
>wB
=qB
?}B
A�B
@�B
>wB
A�B
C�B
C�B
C�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
I�B
I�B
I�B
H�B
G�B
G�B
I�B
I�B
J�B
L�B
L�B
L�B
K�B
I�B
J�B
J�B
M�B
L�B
J�B
L�B
N�B
P�B
O�B
O�B
P�B
P�B
P�B
Q�B
P�B
Q�B
R�B
R�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
R�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
T�B
T�B
S�B
Q�B
O�B
R�B
T�B
T�B
W
B
XB
XB
XB
XB
XB
XB
XB
W
B
VB
VB
T�B
W
B
XB
YB
ZB
ZB
ZB
XB
VB
XB
VB
[#B
]/B
]/B
\)B
ZB
[#B
\)B
]/B
]/B
\)B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
^5B
_;B
_;B
`BB
bNB
aHB
aHB
aHB
bNB
cTB
bNB
bNB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
dZB
dZB
cTB
cTB
dZB
dZB
cTB
bNB
bNB
bNB
bNB
aHB
bNB
dZB
e`B
e`B
e`B
e`B
cTB
dZB
e`B
e`B
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
iyB
hsB
iyB
iyB
iyB
hsB
iyB
jB
iyB
hsB
iyB
hsB
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
m�B
l�B
n�B
o�B
n�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
m�B
n�B
o�B
o�B
p�B
o�B
o�B
n�B
l�B
p�B
r�B
q�B
q�B
p�B
o�B
n�B
o�B
q�B
r�B
q�B
p�B
n�B
o�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
u�B
v�B
v�B
v�B
v�B
u�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
u�B
v�B
u�B
t�B
u�B
w�B
w�B
v�B
v�B
u�B
s�B
u�B
t�B
u�B
w�B
v�B
v�B
u�B
v�B
w�B
x�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
w�B
w�B
w�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
|�B
z�B
{�B
z�B
{�B
z�B
y�B
y�B
{�B
|�B
}�B
}�B
|�B
|�B
}�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
� B
� B
~�B
~�B
~�B
~�B
� B
� B
� B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�+B
�+B
�%B
�%B
�%B
�%B
�+B
�+B
�1B
�+B
�1B
�7B
�7B
�7B
�1B
�1B
�+B
�1B
�7B
�=B
�7B
�7B
�=B
�=B
�7B
�7B
�=B
�=B
�=B
�=B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�DB
�=B
�DB
�=B
�7B
�=B
�DB
�DB
�DB
�PB
�JB
�JB
�DB
�DB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�VB
�bB
�bB
�bB
�bB
�bB
�hB
�oB
�o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                       ! 2 H C . #   > } + #                                  ! % " " $ $                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    5 000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000  PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230421090051                                          AO  ARCAADJP                                                                    20230421090051    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230421090051  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230421090051  QCF$                G�O�G�O�G�O�0               
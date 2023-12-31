CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-06-02T15:35:43Z creation;2017-06-02T15:35:46Z conversion to V3.1;2019-12-18T07:30:25Z update;2022-11-21T05:32:31Z update;     
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
_FillValue                 �  ]d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20170602153543  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               fA   JA  I1_0397_102                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @���I�1   @�!M���@<e+���d���[1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db�fDc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D��D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@,(�@r�\@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'�D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>�D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Db\Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D�D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D��D���D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�9HD�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��HD�9HD�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�E�A�A�A�I�A�M�A�M�A�I�A��7A��#A�1'A�I�A���A��wA�+A���A��DA�hsA���A� �A��/A��;A���A��A���A�~�A��A��A���A�|�A���A�l�A���A��A�K�A�ȴA�A��A�p�A��HA�
=A���A�1'A���A�^5A��A���A�/A��RA���A��A�K�A�l�A�~�A���A��A��A��RA�S�A� �A�"�A�A���A��A��#A�dZA�n�A���A��A�"�A���A��A��hA�~�A���A��
A�33A��HA��A���A�M�A�\)A��!Ap�A~M�A{�^Ay��AwhsAv�yAv�jAvĜAv��Au�FAu"�At�Atv�As�mAq��Ao��AoVAn��AnbAmC�Am�Ak�-Aj�/Aj�DAjZAj{Ah��AgS�Af�AfM�Af�Ae��Ad~�Ab�9Ab(�Aa��A`��A_"�A]�A\�!A\��A\bA[oAZjAY"�AX�uAW�mAV��AU�AT�AR��AR�AQAQ�APVAOK�ANbNAMK�AM;dAL��AK�TAKO�AJr�AI��AI&�AH�DAGl�AF��AE��AE�#AE�-AE|�AEC�ADȴAD{AChsAB�RABJAB  AA`BA@�DA@{A>ĜA=��A=%A<~�A;�TA;K�A:ĜA9p�A8M�A8  A7��A7�wA7�A6�A6^5A5�A5?}A5VA4��A4�uA3��A3
=A2ĜA2��A21'A1A1p�A0�yA0JA/XA.��A.�A-/A,I�A+x�A*�HA*JA(��A((�A'��A'
=A&��A&(�A%��A%/A$��A$(�A#;dA"ffA!x�A �DAƨA�A��A~�A$�A�A�#AC�A�A��A|�A?}AVA�`AjA�PAZA|�A��AJAO�A�9A1'A�A{A+A��Az�A��At�AjAC�A
^5A	�AI�A��A`BA��A&�AA�AȴA�\AVA&�Av�A1'A��A%A V@�{@���@�V@���@�`B@���@�K�@�Ĝ@���@�P@�ȴ@�=q@���@��@��@��@�K�@���@���@��@��@�ȴ@���@ڗ�@��`@֗�@��#@�X@���@Ӿw@��y@�Ĝ@�$�@�7L@�1'@���@�-@�@ɉ7@�`B@�7L@�Z@�dZ@���@őh@¸R@��@��9@��m@�5?@�hs@��j@��@�S�@��@��@�ȴ@�=q@�O�@�bN@��@���@�V@���@�z�@�l�@�@��+@�E�@�-@��@�%@�9X@��@�S�@���@���@�/@�bN@��@�dZ@��T@��P@�n�@�`B@�Z@���@��T@�p�@��@�z�@�l�@��\@��@��@�G�@���@�I�@�1@��m@���@��y@���@��`@��/@��/@��/@��9@�S�@�ȴ@�=q@�V@��/@��9@�bN@�9X@��
@�o@�ff@��-@�7L@�/@�&�@���@��9@�A�@�C�@�
=@��y@��!@���@���@�M�@���@�hs@���@��j@���@��w@���@�S�@�@��@�X@�&�@��@��`@��j@���@�I�@��w@�l�@��@�~�@�M�@��T@���@���@�x�@��@���@��@�ƨ@�C�@���@�~�@�V@�M�@�^5@�5?@�x�@�7L@��`@�;@~��@~��@}�@}`B@}O�@}?}@}�@|��@|�@|�j@|�@{33@z��@y�7@x��@x�9@x��@x�@x  @v�@u�T@u`B@t��@tz�@tz�@t�D@tZ@t�@sdZ@s33@so@s@s"�@r-@rJ@qG�@p��@p1'@o�@o�P@n�y@n�+@m��@m/@m�@l��@lz�@k�
@j�!@jJ@i�7@iG�@i%@h�9@hQ�@h  @g��@g+@fff@f5?@f{@f@e�@e@dI�@c��@co@co@c@c@c@b��@bn�@bM�@bM�@b~�@b��@b��@b�!@b��@b��@b�\@b�@a�@a�@a�#@a�^@ax�@aG�@`�9@`Q�@`A�@`�@`��@`Ĝ@`�@`bN@_�@_K�@^�y@^�R@^E�@]�h@\��@\�@[S�@Z�!@Y�^@XĜ@Xr�@X  @W�w@W��@W|�@WK�@V��@V�+@Vff@VV@VV@V5?@V{@V@U@T�@Tz�@T�D@Tz�@T��@T�/@T�@T�@T��@T�@T�/@T��@T�@TI�@T9X@T1@S�F@S��@SS�@R��@R�\@Rn�@RM�@R�@R�@RJ@Q��@QX@QG�@Q7L@Q�@P��@P��@P�@PQ�@P1'@PA�@PQ�@PQ�@PA�@P �@O�;@O�;@O�@O|�@N��@N@M�@L�@L�@L�@L�@L�j@L�D@L9X@Kƨ@KdZ@J��@JM�@JJ@JJ@J�@I��@I�@H��@H��@H��@H��@Hr�@HQ�@HA�@HA�@H1'@Hb@G�w@G
=@Fv�@F@E@E��@Ep�@E`B@E/@D�@D�@D��@DI�@C��@C��@C33@B��@B-@A�@A�^@AG�@@�9@@bN@@  @?�w@?l�@?\)@?+@?;d@?K�@?
=@>��@>5?@>@=��@=@=��@=�@=`B@=/@<��@<�j@<�@<Z@<1@;S�@:�@:��@:�!@9��@9�7@97L@9&�@8�`@8bN@8A�@7�@7�;@7�@7�P@7l�@7+@7
=@6ȴ@65?@5�-@4��@4�/@4��@4�@4��@49X@3��@2��@2M�@2J@1�#@1��@1�^@1��@1%@0bN@0b@/��@/;d@.ȴ@.ȴ@.�R@.��@.�+@.V@.@-�@-�@-�@-�T@-�-@-��@-O�@,�j@,�@,��@,�D@,z�@,Z@,(�@+��@+ƨ@+�@+@*~�@*-@*�@)�@)�7@)�@(��@(�@(A�@(  @'�;@'��@'�P@'|�@'l�@'\)@';d@'K�@'+@&��@&�y@&ȴ@&�R@&�R@&�R@&ff@%��@%?}@$�@$�@$��@$�D@$j@$Z@$1@#�
@#ƨ@#dZ@"�@"�!@"��@"~�@"=q@!�@!��@!G�@ �9@ �@ r�@ r�@  �@��@|�@K�@+@��@�R@ff@V@V@{@@�@p�@O�@?}@V@��@��@�@��@"�@��@�\@J@x�@G�@&�@%@�`@��@Ĝ@�9@��@�@r�@r�@Q�@A�@A�@ �@�@�;@�;@�w@��@�P@�P@l�@+@�y@��@��@�+@ff@$�@�@�-@p�@O�@/@V@�@�/@�j@�D@(�@��@t�@�@��@n�@n�@^5@�@��@��@X@%@�`@��@�9@��@bN@  @�@�@��@�w@��@��@�P@|�@\)@K�@�@�y@��@v�@ff@ff@V@{@@��@�@`B@�@�/@�j@�D@Z@1@�m@�F@�@S�@33@33@33@
�H@
��@
��@
��@
��@
�!@
�!@
n�@
^5@
^5@
^5@	�@	��@	�^@	��@	��@	��@	�7@	x�@	7L@��@�9@��@��@��@��@�u@Q�@b@b@  @  @�@�;@�;@��@��@�w@��@\)@�@v�@V@5?@{@@�@�T@�T@@�@`B@O�@O�@?}@/@�@V@V@�@�j@�j@�@��@z�@Z@9X@1@�
@��@t�@S�@C�@33@��@=q@�@��@��@�7@X@&�@�@&�@&�@%@%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�E�A�A�A�I�A�M�A�M�A�I�A��7A��#A�1'A�I�A���A��wA�+A���A��DA�hsA���A� �A��/A��;A���A��A���A�~�A��A��A���A�|�A���A�l�A���A��A�K�A�ȴA�A��A�p�A��HA�
=A���A�1'A���A�^5A��A���A�/A��RA���A��A�K�A�l�A�~�A���A��A��A��RA�S�A� �A�"�A�A���A��A��#A�dZA�n�A���A��A�"�A���A��A��hA�~�A���A��
A�33A��HA��A���A�M�A�\)A��!Ap�A~M�A{�^Ay��AwhsAv�yAv�jAvĜAv��Au�FAu"�At�Atv�As�mAq��Ao��AoVAn��AnbAmC�Am�Ak�-Aj�/Aj�DAjZAj{Ah��AgS�Af�AfM�Af�Ae��Ad~�Ab�9Ab(�Aa��A`��A_"�A]�A\�!A\��A\bA[oAZjAY"�AX�uAW�mAV��AU�AT�AR��AR�AQAQ�APVAOK�ANbNAMK�AM;dAL��AK�TAKO�AJr�AI��AI&�AH�DAGl�AF��AE��AE�#AE�-AE|�AEC�ADȴAD{AChsAB�RABJAB  AA`BA@�DA@{A>ĜA=��A=%A<~�A;�TA;K�A:ĜA9p�A8M�A8  A7��A7�wA7�A6�A6^5A5�A5?}A5VA4��A4�uA3��A3
=A2ĜA2��A21'A1A1p�A0�yA0JA/XA.��A.�A-/A,I�A+x�A*�HA*JA(��A((�A'��A'
=A&��A&(�A%��A%/A$��A$(�A#;dA"ffA!x�A �DAƨA�A��A~�A$�A�A�#AC�A�A��A|�A?}AVA�`AjA�PAZA|�A��AJAO�A�9A1'A�A{A+A��Az�A��At�AjAC�A
^5A	�AI�A��A`BA��A&�AA�AȴA�\AVA&�Av�A1'A��A%A V@�{@���@�V@���@�`B@���@�K�@�Ĝ@���@�P@�ȴ@�=q@���@��@��@��@�K�@���@���@��@��@�ȴ@���@ڗ�@��`@֗�@��#@�X@���@Ӿw@��y@�Ĝ@�$�@�7L@�1'@���@�-@�@ɉ7@�`B@�7L@�Z@�dZ@���@őh@¸R@��@��9@��m@�5?@�hs@��j@��@�S�@��@��@�ȴ@�=q@�O�@�bN@��@���@�V@���@�z�@�l�@�@��+@�E�@�-@��@�%@�9X@��@�S�@���@���@�/@�bN@��@�dZ@��T@��P@�n�@�`B@�Z@���@��T@�p�@��@�z�@�l�@��\@��@��@�G�@���@�I�@�1@��m@���@��y@���@��`@��/@��/@��/@��9@�S�@�ȴ@�=q@�V@��/@��9@�bN@�9X@��
@�o@�ff@��-@�7L@�/@�&�@���@��9@�A�@�C�@�
=@��y@��!@���@���@�M�@���@�hs@���@��j@���@��w@���@�S�@�@��@�X@�&�@��@��`@��j@���@�I�@��w@�l�@��@�~�@�M�@��T@���@���@�x�@��@���@��@�ƨ@�C�@���@�~�@�V@�M�@�^5@�5?@�x�@�7L@��`@�;@~��@~��@}�@}`B@}O�@}?}@}�@|��@|�@|�j@|�@{33@z��@y�7@x��@x�9@x��@x�@x  @v�@u�T@u`B@t��@tz�@tz�@t�D@tZ@t�@sdZ@s33@so@s@s"�@r-@rJ@qG�@p��@p1'@o�@o�P@n�y@n�+@m��@m/@m�@l��@lz�@k�
@j�!@jJ@i�7@iG�@i%@h�9@hQ�@h  @g��@g+@fff@f5?@f{@f@e�@e@dI�@c��@co@co@c@c@c@b��@bn�@bM�@bM�@b~�@b��@b��@b�!@b��@b��@b�\@b�@a�@a�@a�#@a�^@ax�@aG�@`�9@`Q�@`A�@`�@`��@`Ĝ@`�@`bN@_�@_K�@^�y@^�R@^E�@]�h@\��@\�@[S�@Z�!@Y�^@XĜ@Xr�@X  @W�w@W��@W|�@WK�@V��@V�+@Vff@VV@VV@V5?@V{@V@U@T�@Tz�@T�D@Tz�@T��@T�/@T�@T�@T��@T�@T�/@T��@T�@TI�@T9X@T1@S�F@S��@SS�@R��@R�\@Rn�@RM�@R�@R�@RJ@Q��@QX@QG�@Q7L@Q�@P��@P��@P�@PQ�@P1'@PA�@PQ�@PQ�@PA�@P �@O�;@O�;@O�@O|�@N��@N@M�@L�@L�@L�@L�@L�j@L�D@L9X@Kƨ@KdZ@J��@JM�@JJ@JJ@J�@I��@I�@H��@H��@H��@H��@Hr�@HQ�@HA�@HA�@H1'@Hb@G�w@G
=@Fv�@F@E@E��@Ep�@E`B@E/@D�@D�@D��@DI�@C��@C��@C33@B��@B-@A�@A�^@AG�@@�9@@bN@@  @?�w@?l�@?\)@?+@?;d@?K�@?
=@>��@>5?@>@=��@=@=��@=�@=`B@=/@<��@<�j@<�@<Z@<1@;S�@:�@:��@:�!@9��@9�7@97L@9&�@8�`@8bN@8A�@7�@7�;@7�@7�P@7l�@7+@7
=@6ȴ@65?@5�-@4��@4�/@4��@4�@4��@49X@3��@2��@2M�@2J@1�#@1��@1�^@1��@1%@0bN@0b@/��@/;d@.ȴ@.ȴ@.�R@.��@.�+@.V@.@-�@-�@-�@-�T@-�-@-��@-O�@,�j@,�@,��@,�D@,z�@,Z@,(�@+��@+ƨ@+�@+@*~�@*-@*�@)�@)�7@)�@(��@(�@(A�@(  @'�;@'��@'�P@'|�@'l�@'\)@';d@'K�@'+@&��@&�y@&ȴ@&�R@&�R@&�R@&ff@%��@%?}@$�@$�@$��@$�D@$j@$Z@$1@#�
@#ƨ@#dZ@"�@"�!@"��@"~�@"=q@!�@!��@!G�@ �9@ �@ r�@ r�@  �@��@|�@K�@+@��@�R@ff@V@V@{@@�@p�@O�@?}@V@��@��@�@��@"�@��@�\@J@x�@G�@&�@%@�`@��@Ĝ@�9@��@�@r�@r�@Q�@A�@A�@ �@�@�;@�;@�w@��@�P@�P@l�@+@�y@��@��@�+@ff@$�@�@�-@p�@O�@/@V@�@�/@�j@�D@(�@��@t�@�@��@n�@n�@^5@�@��@��@X@%@�`@��@�9@��@bN@  @�@�@��@�w@��@��@�P@|�@\)@K�@�@�y@��@v�@ff@ff@V@{@@��@�@`B@�@�/@�j@�D@Z@1@�m@�F@�@S�@33@33@33@
�H@
��@
��@
��@
��@
�!@
�!@
n�@
^5@
^5@
^5@	�@	��@	�^@	��@	��@	��@	�7@	x�@	7L@��@�9@��@��@��@��@�u@Q�@b@b@  @  @�@�;@�;@��@��@�w@��@\)@�@v�@V@5?@{@@�@�T@�T@@�@`B@O�@O�@?}@/@�@V@V@�@�j@�j@�@��@z�@Z@9X@1@�
@��@t�@S�@C�@33@��@=q@�@��@��@�7@X@&�@�@&�@&�@%@%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BBBBBBB�BɺBBŢB��B�jB�B��B��B��B��B��B��B��B��B�PB�7B�+B�7B�bB�hB�\B�=B�B}�Bz�B�B�Bp�B[#BH�B5?B)�B�BJB
=BJB�BB�B��B�oB�+Bw�Bn�B��B�3B��By�BgmBJ�B+BuB
��B
��BB%B1BB
�B
�fB
�`B
�#B
�B
�;B
��B
��B%B�BJB
�ZB
��B
�
B
��B
�^B
��B
�hB
�1B
w�B
l�B
YB
T�B
R�B
R�B
P�B
G�B
B�B
A�B
@�B
?}B
8RB
+B
&�B
#�B
 �B
�B
�B
bB
JB

=B
1B
%B	��B	�B	�B	�B	�sB	�fB	�/B	�B	��B	��B	��B	��B	�?B	�-B	�jB	�jB	�LB	�3B	�B	��B	��B	��B	�\B	�7B	�B	�B	� B	|�B	w�B	n�B	iyB	bNB	cTB	dZB	ZB	T�B	L�B	G�B	E�B	B�B	=qB	9XB	6FB	6FB	5?B	49B	2-B	1'B	0!B	.B	+B	#�B	$�B	"�B	�B	�B	�B	bB	PB	DB	JB	1B	%B	  B��B��B��B��B��B�B�B�B�B�B�yB�mB�TB�BB�;B�BB�5B�)B�B��B��B��B��BȴBÖB�}B�dB�XB�RB�9B�3B�-B�!B�B�B�B��B��B��B��B��B��B��B�uB�bB�PB�7B�1B�+B�%B�B�B~�B}�B}�B|�B{�By�Bv�Bs�Bp�Bo�Bl�BjBhsBffBcTBaHB^5B\)B[#BYBW
BS�BO�BL�BI�BG�BE�BC�BA�B?}B?}B?}B>wB=qB<jB:^B8RB7LB6FB33B1'B/B,B)�B'�B%�B#�B"�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B{BuBoBuBuBuBuBuBuBoBoBuBuBuBuBuBuBuBuBoBuB{BuBuB�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B"�B#�B"�B"�B$�B%�B&�B&�B&�B&�B)�B,B.B/B/B/B0!B2-B1'B2-B5?B<jB?}BB�BE�BK�BO�BP�BQ�BT�BZB_;BaHBdZBe`BffBjBl�Bm�Bn�Br�Bv�By�Bz�Bz�By�By�B�B�B�B�+B�1B�7B�DB�DB�PB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�9B�XB�^B�dB�qB�}BƨB��B��B��B��B��B��B��B��B��B�
B�B�B�)B�/B�/B�;B�HB�ZB�ZB�NB�NB�NB�ZB�ZB�ZB�`B�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	1B	JB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	&�B	&�B	(�B	+B	,B	.B	0!B	33B	6FB	;dB	>wB	B�B	D�B	F�B	G�B	G�B	H�B	H�B	H�B	H�B	H�B	H�B	I�B	J�B	K�B	L�B	L�B	M�B	N�B	O�B	P�B	R�B	VB	W
B	W
B	XB	XB	XB	^5B	aHB	dZB	ffB	ffB	gmB	gmB	gmB	hsB	jB	l�B	n�B	n�B	n�B	o�B	o�B	p�B	q�B	t�B	t�B	u�B	v�B	w�B	x�B	x�B	z�B	}�B	� B	�B	�B	�B	�+B	�+B	�1B	�1B	�1B	�7B	�DB	�\B	�bB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�9B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�dB	�qB	�wB	�}B	��B	��B	B	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�TB	�ZB	�`B	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
1B
	7B
	7B

=B
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
bB
bB
hB
hB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
-B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
1'B
2-B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
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
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
K�B
K�B
L�B
M�B
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
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
T�B
T�B
VB
W
B
XB
XB
XB
YB
XB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
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
^5B
^5B
^5B
^5B
^5B
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
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
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
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
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
l�B
l�B
l�B
l�B
l�B
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
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
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
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BBBGBgB�B�B��B�jB�MB�YB�;B��B��B��B�nB�B�FB�
B��B�WB��B�<B�	B�fB�XB��B��B��B��B��B�B|PB��B�{Bs�B]�BK�B7�B,WB+B�B�BB�B��B�)B�FB�RBx�Bn}B��B��B�bB{�BjBM�B-�BgB
��B
��BBB	�B3B
��B
��B
�RB
�)B
�B
��B
��B
��BtBIB�B
�2B
��B
�_B
� B
�PB
�=B
�[B
�)B
z^B
n�B
Y�B
UMB
SB
S[B
RB
H�B
CB
BAB
A�B
A�B
:�B
+�B
'�B
$�B
!�B
WB
B
NB
�B

�B
�B
�B	��B	�B	�B	��B	�DB	�>B	�B	��B	ԯB	ҽB	͹B	�GB	�+B	��B	��B	��B	�RB	��B	�B	�
B	�TB	�1B	�4B	��B	�?B	��B	��B	~B	y>B	o�B	j�B	b�B	d&B	ezB	[#B	V9B	M�B	H�B	F�B	C�B	>wB	:B	6�B	6�B	5�B	4�B	2�B	2B	1B	/B	+�B	$ZB	%�B	#�B	�B	]B	�B	NB	"B	JB	6B		RB	�B	;B�dB�DB�>B�lB��B�B��B�B��B��B�0B�B�B��BߤB��B��B��B�B�9B��BοB��B�	B��B��B��B��B��B�tB�B�B��B��B��B��B��B�
B�,B�B�B�B��B��B��B��B��B��B��B�+B�B�GBHB~wB~]B}�B|�B{JBxlBt�Bq�Bp�Bm�BkkBiyBh$Bd�Bb�B_B\�B\BZBX�BU�BQhBNpBJ�BH�BF�BD�BC-B?�B?�B?�B>�B>BB=�B;dB8�B8B7�B4�B2�B0�B-]B+�B)�B&�B%B$@B"hB"hB!|B vB �B!bB �B�BVB�B5BIBWB�BSB{B�B�BB�B�BFBFBB�BaBaBaB�B�B�B�B�B&BFB2B�BgB�BB_B�BWB]BdB!BBB 'BVB vB!�B"�B"�B#nB$@B#nB#�B%FB&LB'RB'B'RB'�B*�B,�B.cB/�B/�B/�B0�B2�B1�B3�B6�B=VB@iBC�BF�BL~BPHBQhBR�BU�BZ�B_�Ba�Bd�Be�Bf�Bj�Bl�Bm�Bo�Bs�BwLBy�B{B{BzDBz�B�oB��B��B�zB�fB��B��B��B��B��B�B�B��B��B��B�B�OB�vB�B�B�8B�
B�*B�_B�eB��B��B��B��B��B��B��B��B�4B�+B��B��B��B�B�B�"B�HB�FB�gB�sB�KB�kB�]B�dB�~BߊB��B�B��B�B�B�B�B�B�tB�B�
B��B�B�IB��B��B�B�	B�B��B��B�B�"B�(B	 OB	uB	�B	�B	�B	�B	�B	�B	�B	B	B	�B	�B	�B	 �B	"�B	$�B	'8B	'RB	)*B	+6B	,"B	.IB	0oB	3hB	6�B	;�B	>�B	B�B	D�B	GB	G�B	HB	IB	H�B	H�B	H�B	IB	IB	J	B	J�B	K�B	MB	MB	NB	O(B	P.B	QB	S[B	VB	W$B	W?B	X+B	X_B	X�B	^jB	a|B	dtB	ffB	f�B	g�B	g�B	g�B	h�B	jB	l�B	n�B	n�B	n�B	o�B	o�B	p�B	q�B	t�B	t�B	u�B	v�B	xB	y	B	y$B	{B	}�B	�B	�'B	�3B	�9B	�_B	�_B	��B	��B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	��B	�B	�$B	�B	�0B	�0B	�eB	�)B	�!B	�-B	�B	�9B	�9B	�ZB	�`B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	�B	�B	��B	�B	��B	� B	��B	�B	�B	�&B	�B	�,B	�B	�,B	�gB	�SB	�yB	�KB	�B	�)B	�5B	�VB	�vB	�|B	�B	�B	�B	�B	�tB	�zB	�mB	�B	��B	�B	�sB	�B	�B	�B	�B	�B	��B	��B	�B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�*B	�B	�(B	�B
 OB
 OB
 B
AB
MB
3B
SB
YB
KB
	7B
	RB

�B
~B
dB
�B
�B
jB
�B
pB
pB
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
 'B
!-B
#B
#B
$B
$B
%B
$�B
%,B
&B
(
B
($B
($B
)B
*B
*0B
*0B
*B
*0B
+B
+B
+B
+B
+B
+6B
,=B
,WB
-CB
.B
./B
./B
/B
/OB
/OB
/OB
0UB
0;B
1vB
2aB
3hB
4nB
4TB
4nB
4nB
5tB
5tB
6zB
6`B
7�B
7fB
7fB
8lB
8RB
8lB
8lB
8RB
8lB
8�B
9XB
9�B
9rB
9XB
9rB
9�B
9�B
:�B
;�B
;�B
<jB
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
IB
I�B
J	B
J�B
K�B
K�B
MB
NB
OB
OB
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
PB
O�B
O�B
O�B
O�B
O�B
O�B
QB
QB
Q B
Q B
Q B
QB
RB
R B
Q�B
RB
RB
S&B
SB
SB
S&B
TB
T,B
T,B
TB
TB
T,B
TB
U2B
UB
VSB
W?B
XEB
XEB
X+B
YKB
X+B
YKB
Y1B
Y1B
ZQB
Z7B
Z7B
[WB
[WB
[WB
[=B
\)B
\CB
\]B
\CB
\CB
\CB
\CB
\CB
]IB
]dB
]IB
]IB
]IB
^jB
^OB
^5B
^OB
^jB
_;B
_pB
_pB
`\B
`\B
`\B
a|B
a|B
a|B
a|B
bhB
bhB
bhB
bhB
cTB
cnB
cTB
cnB
cnB
cTB
cTB
cTB
cnB
cTB
cnB
dtB
dZB
dtB
dtB
e�B
ezB
ezB
e`B
e`B
ezB
ezB
e�B
f�B
f�B
g�B
gmB
gmB
gmB
g�B
g�B
g�B
h�B
h�B
h�B
hsB
hsB
h�B
h�B
hsB
h�B
h�B
i�B
i�B
j�B
j�B
j�B
j�B
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
l�B
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
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
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
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201706140032232017061400322320170614003223202211182130342022111821303420221118213034201804031935562018040319355620180403193556  JA  ARFMdecpA19c                                                                20170603003509  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170602153543  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170602153545  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170602153545  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170602153546  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170602153546  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170602153546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170602153546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170602153546  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170602153546                      G�O�G�O�G�O�                JA  ARUP                                                                        20170602160646                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170602153218  CV  JULD            G�O�G�O�F�`�                JM  ARCAJMQC2.0                                                                 20170613153223  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170613153223  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103556  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171534                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123034  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                
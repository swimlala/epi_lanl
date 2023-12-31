CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-06-12T18:35:57Z creation;2017-06-12T18:36:00Z conversion to V3.1;2019-12-18T07:30:12Z update;2022-11-21T05:32:28Z update;     
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
_FillValue                 �  ]   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͸   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170612183557  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               gA   JA  I1_0397_103                     2C  Ddq�NAVIS_A                         0397                            ARGO 011514                     863 @��<}� 1   @���� @;b�7��4�dq��R1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Ǯ@x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW�=CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW�\DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�yHD��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D��D���D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D���D��{D�<{D�|{D��{D��{D�<{D�|{D��HD��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D��D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��A�VA�ZA�VA�Q�AąA�ffA�jA�`BA��7A�VA�S�A��yA�A���A�A�A�1'A��TA�&�A��7A�x�A�A��9A��HA�K�A���A��A��7A�33A��/A��
A�ZA�ƨA���A�$�A���A��PA�VA���A���A���A�=qA��A��A�ZA�p�A���A���A�"�A�1A�+A�=qA�A�O�A�C�A�|�A��mA��A�VA��\A���A��DA��!A��A���A��RA�1A�hsA�p�A��A�~�A��#A�C�A��A�&�A���A��7A��A}�TA{�mAzv�AyAw��Aul�At��As��Ar~�AqO�Ao�wAn��AnjAm�#Ak�AjA�Aj  Ai�^Ai�Ai?}Ah9XAgdZAgAf �AeO�Ae�Ad��Ad  Ac�7Ab�Ab1AaAa��Aap�A`n�A_+A^ffA]/A\^5A[?}AY�^AX1'AW&�AV�AVbAU�AUC�AT��AS��AS`BAR��AQ�AP��API�AO��AN��AN-AMp�ALbAJ�`AJE�AI�7AHĜAH^5AHZAG��AF�jAFJAE��AEVADJAC�hAC\)AB�/AB1'AA\)A@��A@(�A?|�A>�A>A=?}A=�A<ĜA<  A;��A;+A:^5A9`BA8=qA7?}A6z�A6JA5O�A4M�A4  A3��A2��A2(�A1�A1x�A133A0�A05?A/�;A/�7A/
=A.�A,�A+/A*�A*�A)��A)%A(��A(bNA'��A&��A&E�A%`BA$~�A#ƨA"��A!ƨA!��A!C�A �A {An�AȴA7LAA�A��A�;A�hAG�AoAȴA�!A1'AS�A%A�jAbNA�A�A��A��A�7A��A7LAA�AbA  AA�TAK�A	��AĜAE�A�mA/AQ�A��Al�A33A�\Al�AI�A�Ax�A �yA  �@��@��@��H@���@��F@��H@�~�@�{@�?}@�@�l�@��@��
@�@���@�x�@�Z@�b@�S�@�M�@��#@��@�X@�Ĝ@�A�@�|�@��@�%@��y@�M�@��#@��@ܛ�@�(�@۝�@ٺ^@��@أ�@�bN@�1'@��@�1@ם�@�7L@�+@�r�@�$�@�(�@�%@�  @�C�@�-@�`B@�Q�@�+@�=q@�p�@��@�ff@��+@�dZ@�ȴ@��@���@�X@���@�r�@�bN@�I�@� �@�  @���@���@���@�ff@��-@�&�@��@��@���@�E�@�J@��@���@��m@��\@��@���@�S�@��R@�J@���@���@��@�j@�A�@�1@��;@�S�@�o@��@��!@���@��!@���@�X@�b@�"�@�v�@�V@�O�@�ȴ@��\@��H@�o@�dZ@�t�@�ff@�-@�-@���@�`B@�hs@���@��y@�n�@��-@��`@���@��@�&�@�&�@�7L@�G�@��@��@��#@��+@��T@�M�@���@��F@���@��H@�V@� �@���@��7@�b@��
@�dZ@��@��\@�hs@�I�@�b@�I�@�I�@�I�@�r�@�1'@��
@���@��F@���@�/@��@���@�7L@��@�x�@�O�@�9X@��@�M�@��@�@�/@��/@���@�%@�7L@�?}@�&�@��9@�r�@�I�@� �@�dZ@�&�@�Z@���@��@�w@\)@~��@~�y@\)@l�@\)@
=@~�y@~ȴ@~v�@~5?@}��@}O�@|��@|�@|�j@|�@{�m@{��@{C�@{"�@z�@z��@z��@zn�@x�9@v�R@v$�@u�-@u@t��@tj@tI�@sdZ@so@r�\@o��@o
=@n�@oK�@p1'@n�R@m/@l�@n{@r��@rM�@q��@pĜ@pr�@p�`@qG�@qhs@nv�@l�@l�@l�@j�\@h �@fV@f$�@f��@i��@j�H@j�@i��@ix�@i�7@ix�@h�9@f��@e?}@d(�@cdZ@bM�@b��@b=q@`�`@`�u@`bN@`bN@`�@`1'@_|�@^�R@]��@\��@\�D@\j@\j@\j@[�
@[�@[33@Z�H@Z��@Z��@[t�@\�@\�j@\�/@\j@[��@[33@[o@[@[�@[�
@\�@\�j@\9X@[��@Z��@Y�@Xr�@XA�@W��@V�R@Vff@V{@U@U�@U�@Up�@T��@TI�@T�@S�
@S�F@SdZ@SS�@SS�@SS�@SS�@St�@S�@St�@St�@SdZ@R-@P��@P�@P1'@PA�@O�w@O�w@O�@O�@O�P@O;d@O�@O
=@N�@N��@Nv�@N5?@M�@M�T@M�-@M`B@L�@L��@LI�@K�m@K�@K"�@J�H@J��@J^5@J�@I�@I�^@Ihs@I�@HbN@G�@G|�@G+@F��@F�@F�R@F�+@Fff@F$�@F@E�T@E��@E�-@E��@E�h@E`B@D��@D�@Dj@D1@Ct�@Co@B�!@B~�@B^5@BM�@A�@Ahs@A&�@@�`@@Ĝ@@�9@@�@@A�@?�@?�;@?�P@?K�@>�R@>E�@=�@=��@=p�@=O�@=V@<�@<j@<I�@<1@;�m@;C�@:��@:�!@:��@:�\@:~�@:^5@:�@9��@9��@9hs@8��@8��@8r�@7�@7+@7
=@6��@6E�@6$�@6@5�T@5@5p�@5?}@4�j@49X@3�F@3C�@3o@2��@2n�@2M�@1�@1��@1�7@1X@1&�@0�`@0�9@0r�@0A�@/�@/��@/|�@/|�@/|�@/l�@/l�@/K�@/
=@.��@.�+@.ff@-�@-�h@-O�@-/@,�/@,�D@,Z@,9X@,9X@,�@+��@+��@+S�@+S�@+33@+33@*��@*~�@*^5@*=q@*J@)��@)��@)��@)�7@)hs@)X@)X@)7L@)&�@(��@(�9@'��@'��@'��@'��@'�P@'|�@'l�@'
=@&��@&@%��@%��@%`B@%?}@%/@$�/@$j@$9X@$(�@$(�@$(�@$�@$�@$�@$1@#�m@#�
@#�@#"�@"��@"~�@"^5@"=q@"-@"-@!�@!��@!X@!X@!X@!X@!X@!&�@!%@ �9@ �@��@;d@�@��@�y@ȴ@��@5?@{@�-@�@O�@��@�D@Z@9X@�@1@��@ƨ@C�@"�@@��@��@�\@^5@�@�7@��@�u@1'@b@�@�;@�w@�P@|�@\)@�y@ff@$�@{@@@@p�@/@V@V@��@��@�D@j@9X@�F@S�@"�@�@�H@��@�!@^5@��@�#@�^@��@�7@hs@X@%@��@Ĝ@r�@A�@1'@�@��@��@�P@+@�y@�R@ff@5?@{@�@��@�-@�-@�h@�@`B@V@�@�/@��@��@�j@�D@j@j@j@Z@9X@(�@1@��@dZ@33@
�@
�\@
M�@
-@
J@	��@	x�@	hs@	G�@	&�@	�@	%@��@��@bN@Q�@  @�@�@�;@�w@��@\)@
=@�@�@�@�R@�+@ff@V@E�@E�@E�@5?@{@{@$�@{@��@��@�@`B@/@/@��@�D@Z@(�@��@�
@ƨ@�F@��@��@�@t�@33@�!@^5@M�@=q@�@��@��@hs@X@&�@ �`@ Ĝ@ Ĝ@ ��@ �@ bN@ Q�@ 1'@  �?��;?��w?���?�|�?�|�?�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��A�VA�ZA�VA�Q�AąA�ffA�jA�`BA��7A�VA�S�A��yA�A���A�A�A�1'A��TA�&�A��7A�x�A�A��9A��HA�K�A���A��A��7A�33A��/A��
A�ZA�ƨA���A�$�A���A��PA�VA���A���A���A�=qA��A��A�ZA�p�A���A���A�"�A�1A�+A�=qA�A�O�A�C�A�|�A��mA��A�VA��\A���A��DA��!A��A���A��RA�1A�hsA�p�A��A�~�A��#A�C�A��A�&�A���A��7A��A}�TA{�mAzv�AyAw��Aul�At��As��Ar~�AqO�Ao�wAn��AnjAm�#Ak�AjA�Aj  Ai�^Ai�Ai?}Ah9XAgdZAgAf �AeO�Ae�Ad��Ad  Ac�7Ab�Ab1AaAa��Aap�A`n�A_+A^ffA]/A\^5A[?}AY�^AX1'AW&�AV�AVbAU�AUC�AT��AS��AS`BAR��AQ�AP��API�AO��AN��AN-AMp�ALbAJ�`AJE�AI�7AHĜAH^5AHZAG��AF�jAFJAE��AEVADJAC�hAC\)AB�/AB1'AA\)A@��A@(�A?|�A>�A>A=?}A=�A<ĜA<  A;��A;+A:^5A9`BA8=qA7?}A6z�A6JA5O�A4M�A4  A3��A2��A2(�A1�A1x�A133A0�A05?A/�;A/�7A/
=A.�A,�A+/A*�A*�A)��A)%A(��A(bNA'��A&��A&E�A%`BA$~�A#ƨA"��A!ƨA!��A!C�A �A {An�AȴA7LAA�A��A�;A�hAG�AoAȴA�!A1'AS�A%A�jAbNA�A�A��A��A�7A��A7LAA�AbA  AA�TAK�A	��AĜAE�A�mA/AQ�A��Al�A33A�\Al�AI�A�Ax�A �yA  �@��@��@��H@���@��F@��H@�~�@�{@�?}@�@�l�@��@��
@�@���@�x�@�Z@�b@�S�@�M�@��#@��@�X@�Ĝ@�A�@�|�@��@�%@��y@�M�@��#@��@ܛ�@�(�@۝�@ٺ^@��@أ�@�bN@�1'@��@�1@ם�@�7L@�+@�r�@�$�@�(�@�%@�  @�C�@�-@�`B@�Q�@�+@�=q@�p�@��@�ff@��+@�dZ@�ȴ@��@���@�X@���@�r�@�bN@�I�@� �@�  @���@���@���@�ff@��-@�&�@��@��@���@�E�@�J@��@���@��m@��\@��@���@�S�@��R@�J@���@���@��@�j@�A�@�1@��;@�S�@�o@��@��!@���@��!@���@�X@�b@�"�@�v�@�V@�O�@�ȴ@��\@��H@�o@�dZ@�t�@�ff@�-@�-@���@�`B@�hs@���@��y@�n�@��-@��`@���@��@�&�@�&�@�7L@�G�@��@��@��#@��+@��T@�M�@���@��F@���@��H@�V@� �@���@��7@�b@��
@�dZ@��@��\@�hs@�I�@�b@�I�@�I�@�I�@�r�@�1'@��
@���@��F@���@�/@��@���@�7L@��@�x�@�O�@�9X@��@�M�@��@�@�/@��/@���@�%@�7L@�?}@�&�@��9@�r�@�I�@� �@�dZ@�&�@�Z@���@��@�w@\)@~��@~�y@\)@l�@\)@
=@~�y@~ȴ@~v�@~5?@}��@}O�@|��@|�@|�j@|�@{�m@{��@{C�@{"�@z�@z��@z��@zn�@x�9@v�R@v$�@u�-@u@t��@tj@tI�@sdZ@so@r�\@o��@o
=@n�@oK�@p1'@n�R@m/@l�@n{@r��@rM�@q��@pĜ@pr�@p�`@qG�@qhs@nv�@l�@l�@l�@j�\@h �@fV@f$�@f��@i��@j�H@j�@i��@ix�@i�7@ix�@h�9@f��@e?}@d(�@cdZ@bM�@b��@b=q@`�`@`�u@`bN@`bN@`�@`1'@_|�@^�R@]��@\��@\�D@\j@\j@\j@[�
@[�@[33@Z�H@Z��@Z��@[t�@\�@\�j@\�/@\j@[��@[33@[o@[@[�@[�
@\�@\�j@\9X@[��@Z��@Y�@Xr�@XA�@W��@V�R@Vff@V{@U@U�@U�@Up�@T��@TI�@T�@S�
@S�F@SdZ@SS�@SS�@SS�@SS�@St�@S�@St�@St�@SdZ@R-@P��@P�@P1'@PA�@O�w@O�w@O�@O�@O�P@O;d@O�@O
=@N�@N��@Nv�@N5?@M�@M�T@M�-@M`B@L�@L��@LI�@K�m@K�@K"�@J�H@J��@J^5@J�@I�@I�^@Ihs@I�@HbN@G�@G|�@G+@F��@F�@F�R@F�+@Fff@F$�@F@E�T@E��@E�-@E��@E�h@E`B@D��@D�@Dj@D1@Ct�@Co@B�!@B~�@B^5@BM�@A�@Ahs@A&�@@�`@@Ĝ@@�9@@�@@A�@?�@?�;@?�P@?K�@>�R@>E�@=�@=��@=p�@=O�@=V@<�@<j@<I�@<1@;�m@;C�@:��@:�!@:��@:�\@:~�@:^5@:�@9��@9��@9hs@8��@8��@8r�@7�@7+@7
=@6��@6E�@6$�@6@5�T@5@5p�@5?}@4�j@49X@3�F@3C�@3o@2��@2n�@2M�@1�@1��@1�7@1X@1&�@0�`@0�9@0r�@0A�@/�@/��@/|�@/|�@/|�@/l�@/l�@/K�@/
=@.��@.�+@.ff@-�@-�h@-O�@-/@,�/@,�D@,Z@,9X@,9X@,�@+��@+��@+S�@+S�@+33@+33@*��@*~�@*^5@*=q@*J@)��@)��@)��@)�7@)hs@)X@)X@)7L@)&�@(��@(�9@'��@'��@'��@'��@'�P@'|�@'l�@'
=@&��@&@%��@%��@%`B@%?}@%/@$�/@$j@$9X@$(�@$(�@$(�@$�@$�@$�@$1@#�m@#�
@#�@#"�@"��@"~�@"^5@"=q@"-@"-@!�@!��@!X@!X@!X@!X@!X@!&�@!%@ �9@ �@��@;d@�@��@�y@ȴ@��@5?@{@�-@�@O�@��@�D@Z@9X@�@1@��@ƨ@C�@"�@@��@��@�\@^5@�@�7@��@�u@1'@b@�@�;@�w@�P@|�@\)@�y@ff@$�@{@@@@p�@/@V@V@��@��@�D@j@9X@�F@S�@"�@�@�H@��@�!@^5@��@�#@�^@��@�7@hs@X@%@��@Ĝ@r�@A�@1'@�@��@��@�P@+@�y@�R@ff@5?@{@�@��@�-@�-@�h@�@`B@V@�@�/@��@��@�j@�D@j@j@j@Z@9X@(�@1@��@dZ@33@
�@
�\@
M�@
-@
J@	��@	x�@	hs@	G�@	&�@	�@	%@��@��@bN@Q�@  @�@�@�;@�w@��@\)@
=@�@�@�@�R@�+@ff@V@E�@E�@E�@5?@{@{@$�@{@��@��@�@`B@/@/@��@�D@Z@(�@��@�
@ƨ@�F@��@��@�@t�@33@�!@^5@M�@=q@�@��@��@hs@X@&�@ �`@ Ĝ@ Ĝ@ ��@ �@ bN@ Q�@ 1'@  �?��;?��w?���?�|�?�|�?�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111116FB�#B�#B�#B�B�B�fB�fB�HB�#B��B��B��B��B�}BǮBƨBȴBɺBĜB�^B�B��B��B�\By�BffB_;BXBN�BI�B=qB49B6FB:^B=qB=qB:^B6FB&�B�B%B��B�B�B�HB�B��B�qB�-B��B��B�oB�DB�B~�Bx�Bs�Bn�BffBZBK�B@�B<jB1'B#�B�BVB
��B
�B
�ZB
�)B
��B
��B
ĜB
�jB
�B
��B
��B
�7B
� B
u�B
k�B
_;B
YB
Q�B
I�B
A�B
7LB
1'B
.B
(�B
�B
{B
oB
bB
VB
JB
%B
  B	��B	��B	�B	�B	�B	�sB	�ZB	�HB	�BB	�;B	�5B	�)B	�B	��B	��B	ÖB	�wB	�LB	�B	��B	��B	��B	��B	��B	��B	�hB	�PB	�7B	�B	�B	|�B	y�B	v�B	q�B	l�B	hsB	aHB	[#B	XB	S�B	P�B	N�B	M�B	J�B	D�B	A�B	?}B	<jB	7LB	49B	2-B	.B	(�B	"�B	�B	�B	�B	�B	hB	DB	PB	DB	+B	+B	%B	B��B��B�B�B�B�yB�fB�fB�ZB�HB�5B�/B�#B�B�B��B��B��B��B��BĜB�qB�jB�^B�FB�9B�-B�'B�B�B��B��B��B��B��B��B��B��B�{B�hB�DB�+B�B}�By�Bw�Bv�Bu�Bt�Bs�Br�Bq�Bo�Bm�Bl�Bk�BhsBffBcTBcTBbNB_;B\)BZBYBW
BT�BQ�BN�BK�BI�BG�BF�BD�BB�BA�B@�B>wB=qB:^B8RB7LB6FB49B2-B0!B.B-B,B+B)�B(�B(�B'�B%�B$�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B#�B#�B#�B#�B#�B"�B#�B �B �B�B�B�B �B(�B.B/B0!B7LB6FB5?B2-B1'B6FB<jB;dB:^B=qB?}BA�BC�BD�BD�BD�BD�BC�BA�B@�BC�BH�BO�BP�BQ�BP�BP�BP�BP�BP�BS�BYB[#B]/BffBhsBiyBm�Bq�Bx�B�B�B�+B�1B�7B�=B�=B�=B�VB�hB��B��B��B��B��B��B��B�uB��B��B��B��B�B�-B�3B�-B�-B�-B�3B�-B�!B�!B�'B�-B�?B�FB�LB�LB�RB�dB�qB�jB�FB�!B�-B�^BĜBŢBŢBĜB��B�}B��B�
B�B�
B�B��B�B�B��B�B�#B�TB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	%B	1B	1B	
=B	
=B	JB	JB	1B	1B	DB	JB	DB	DB	PB	uB	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	&�B	,B	-B	.B	/B	/B	1'B	49B	6FB	7LB	7LB	;dB	:^B	:^B	:^B	9XB	9XB	:^B	<jB	<jB	=qB	<jB	<jB	>wB	A�B	C�B	D�B	G�B	J�B	I�B	G�B	J�B	O�B	]/B	`BB	`BB	_;B	aHB	e`B	gmB	iyB	hsB	ffB	ffB	e`B	bNB	_;B	]/B	aHB	hsB	p�B	t�B	u�B	v�B	x�B	z�B	|�B	~�B	|�B	z�B	y�B	w�B	w�B	{�B	}�B	}�B	~�B	�B	�B	�B	�B	�+B	�7B	�VB	�bB	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�?B	�LB	�XB	�^B	�XB	�RB	�XB	�LB	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�FB	�LB	�XB	�^B	�^B	�jB	�wB	�}B	��B	��B	��B	ÖB	ŢB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
1B
	7B

=B
DB
DB
JB
PB
VB
VB
\B
\B
\B
hB
oB
oB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
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
1'B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
;dB
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
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
N�B
N�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
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
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
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
^5B
^5B
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
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
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
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
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
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111116FB�=B�qB�)B�B޸B��B�*B�B�BԯB�4B�BðB�[BɺB��BʌBˬB�_B��B��B��B�B��B~(BiBbNB[#BQhBLdB>�B4�B7LB;0B>BB>�B;�B8�B)�B�B�B�"B��B�B�TB��B�hB� B�nB�kB�bB�aB��B��B�OBy�Bt�Bp!Bh�B\xBM�BA�B>]B3�B%�BqB B�B
��B
��B
��B
՛B
�B
�YB
��B
� B
�B
��B
�B
��B
w�B
m�B
`vB
Z�B
SuB
KDB
CaB
8lB
1�B
/OB
+QB
5B
�B
�B
�B
�B
�B
B
 �B	�B	��B	�B	�[B	�B	�*B	�`B	�4B	�B	ߊB	��B	ݘB	רB	�B	�0B	��B	� B	�XB	��B	�B	�]B	�qB	��B	�eB	��B	�oB	�B	�=B	�?B	�'B	}�B	z�B	w�B	r�B	m�B	jB	b�B	\)B	X�B	T�B	Q�B	O(B	N�B	K�B	E�B	BAB	@iB	=�B	8B	4�B	2�B	/ B	)�B	#�B	�B	�B	�B	mB	:B	�B	�B	0B	�B	�B	EB	{B�wB�	B�B�B�B�B�B�B�zB��B޸B��BۦB��BּB՛BӏBҽB��B�"B�tB�B�"B�B�2B��B��B�B��B��B�KB�B�B�B�B�B�EB��B��B��B��B�B��B�Bz�BxRBw2Bv+Bu?Bt9Bs�Br�Bp!BnIBmCBl�Bi�BgmBc�Bc�Bc�BaB]dBZ�BY�BXyBV�BS&BP�BMBJ�BHfBG�BE�BCaBB'BA;B?�B?B;�B9$B88B7LB5�B4B1'B/B.B-CB+�B*eB)�B)�B(�B'B&�B#�B"�B"hB �B�B;BjBjBBBB5B/B~BxB�B�BCBCBCBB)BxB�B \B"B$&B$B$&B$@B$�B$�B%�B"�B"�BjBqB~B!|B)�B.�B0B1'B8B7B6zB2�B1AB6+B=B<B:�B=�B@BA�BC�BD�BD�BEBESBD�BB�BA�BD3BIlBP�BQhBRoBQNBQNBQNBQ�BQ�BU2BZ7B\B]�Bf�Bh�Bi�Bm�BraBy$B�AB�SB�zB��B��B�rB�rB�rB��B�:B�)B��B�dB�IB�	B��B��B��B��B��B��B�,B��B�|B�MB�|B�|B�|B��B��B��B��B��B�-B�ZB�`B�fB��B��B��B�]B�]B�8B��B��B��BĜB�B�tBżB� B�HB��B��B�SB�sBևB՛B��BּB�MB�B�=B�nB�B��B��B��B�'B��B�B��B�B��B��B�6B�<B��B��B��B�B�B�B�*B�B	B	B	GB	tB	�B	�B	
rB	
�B	B	�B	�B	1B	�B	�B	xB	xB	jB	uB	�B	�B	�B	�B	�B	B	!B	!�B	$&B	'8B	,=B	-CB	.cB	/OB	/iB	1[B	4TB	6`B	7fB	7�B	;�B	;0B	;B	:�B	9�B	9�B	:�B	<�B	<�B	=�B	<�B	<�B	?cB	A�B	C�B	D�B	G�B	K^B	J=B	G�B	J#B	N�B	]dB	`�B	`�B	_VB	aHB	ezB	g�B	jB	h�B	f�B	f�B	f2B	c:B	_�B	]IB	`�B	g�B	pUB	t�B	vB	v�B	x�B	{0B	}VB	�B	}�B	{dB	zDB	x8B	w�B	|B	~wB	~(B	.B	� B	�AB	�gB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�|B	��B	�B	�"B	�CB	�]B	�IB	�IB	�;B	�'B	�?B	�B	��B	��B	��B	��B	��B	��B	�tB	��B	��B	�tB	�ZB	�tB	�tB	�tB	�ZB	��B	��B	��B	�xB	��B	��B	�wB	��B	��B	��B	��B	ÖB	ŢB	��B	��B	�1B	�7B	��B	��B	�B	�B	��B	��B	�B	�&B	�2B	�?B	�+B	�EB	�1B	�QB	�=B	�]B	�CB	�]B	�dB	�jB	�pB	�vB	�vB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�"B	�"B	�<B	�HB
 4B
 B
AB
AB
'B
GB
GB
3B
gB
SB
YB
fB
	lB

rB
^B
xB
~B
jB
�B
pB
�B
vB
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
 �B
"B
"�B
#B
$B
$B
%B
%�B
%�B
'B
'B
'B
(
B
(
B
)B
)B
)*B
)B
)�B
)B
)�B
*B
*B
*0B
*0B
+6B
+B
,=B
,=B
-CB
-CB
-CB
.IB
./B
/5B
/B
/OB
/5B
/5B
0;B
1AB
1AB
1AB
1[B
2aB
2GB
3MB
3MB
49B
4nB
4nB
4nB
5tB
5ZB
5?B
5tB
5ZB
5tB
6zB
6�B
7�B
8RB
8lB
8lB
8lB
8�B
8�B
9�B
:�B
;�B
;B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>wB
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
EB
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
MB
MB
L�B
M�B
M�B
M�B
NB
O(B
O(B
PB
QB
R B
RB
RB
R B
RB
RB
R B
R:B
S@B
T,B
TB
S�B
TB
TB
TB
UB
U2B
T�B
U2B
U2B
VB
V9B
VB
W?B
W?B
XEB
X+B
XB
XEB
X+B
XEB
YKB
ZQB
Z7B
Z7B
[=B
[=B
[=B
[WB
[=B
[=B
[WB
\]B
\]B
\]B
\CB
\CB
]IB
]dB
]dB
^OB
^OB
^jB
^jB
^OB
_VB
_VB
_VB
_pB
_pB
_VB
_pB
`\B
`BB
`\B
`BB
`\B
`vB
abB
abB
abB
abB
abB
abB
a|B
a|B
b�B
b�B
b�B
cnB
c�B
dtB
dtB
d�B
ezB
ezB
ezB
e�B
e�B
ffB
f�B
f�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
j�B
jB
jB
jB
j�B
jB
jB
j�B
j�B
j�B
k�B
k�B
k�B
l�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
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
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201706230031242017062300312420170623003124202211182130422022111821304220221118213042201804031936042018040319360420180403193604  JA  ARFMdecpA19c                                                                20170613033520  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170612183557  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170612183558  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170612183558  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170612183559  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170612183559  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170612183559  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170612183559  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170612183559  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170612183600                      G�O�G�O�G�O�                JA  ARUP                                                                        20170612191408                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170613153147  CV  JULD            G�O�G�O�F�u                JM  ARGQJMQC2.0                                                                 20170613153147  CV  JULD_LOCATION   G�O�G�O�F�u0                JM  ARGQJMQC2.0                                                                 20170613153147  CV  LATITUDE        G�O�G�O�A�J                JM  ARGQJMQC2.0                                                                 20170613153147  CV  LONGITUDE       G�O�G�O��#�P                JM  ARCAJMQC2.0                                                                 20170622153124  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170622153124  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103604  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171534                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123042  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                
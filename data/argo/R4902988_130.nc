CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-05-02T09:42:17Z creation;2023-05-02T09:42:18Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230502094217  20230502095901  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�(F��61   @�(��X�@;���n��c��O�;d1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�33A��A   A@  A^ffA~ffA�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC�fC
  C�C�C  C  C�fC  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cq�fCt�Cv  Cx  Cz  C{�fC~  C�  C��C�  C�  C��C��3C�  C�  C��C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C��C�  C�  C�  C��C��3C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C��C��C��C�  C��3C��3C��3D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	y�D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D��D� D  D� D��D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db�fDcfDc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk�fDl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dry�Ds  Ds� Dt  Dt� Du  Du� DvfDv�fDw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� D}fD}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D���D�<�D�� D���D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D��3D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�<�D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�|�D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D��3D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D���D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЃ3D�� D�  D�@ Dр D�� D�  D�C3DҀ D�� D�  D�C3DӀ D�� D�  D�<�DԀ D�� D�  D�@ DՀ D��3D�  D�@ Dր Dּ�D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�<�D�|�D�� D�3D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D���D�<�D�|�D�� D�  D�@ D� D�� D�  D�C3D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�|�D�� D�  D�@ D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��G@��@��A=qA>=qA\��A|��A��A��A��A�Q�A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C�=C�=C	��C�qC�qC��C��C�=C��C��C��C��C��C��C!��C#��C%�=C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG�qCI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_�qCa��Cc��Ce�=Cg��Ci��Ck��Cm��Co��Cq�=Cs�qCu��Cw��Cy��C{�=C}��C��C���C���C���C���C��C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C��C���C���C���C���C���C���C���C��C��C���C���C���C��C��C���C���C���C���C���C���C���C��C��C��C���C���C���C���C���C���C���C��C��C��C���D \D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	r�D	��D
x�D
��Dx�D��Dx�D�\Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D\D��Dx�D��Dx�D��Dx�D�Dx�D��Dx�D�Dx�D��Dx�D��Dx�D��D x�D ��D!r�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)�D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4\D4��D5x�D5��D6x�D6��D7x�D7�D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<�\D=x�D=��D>x�D>��D?x�D?�D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIr�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP�DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\\D\��D]x�D]��D^\D^��D_x�D_��D`x�D`��Dax�Da��Db\Db�\Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Dj\Dj��Dk\Dk��Dlx�Dl��Dmr�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqr�Dq��Drr�Dr��Dsx�Ds��Dtx�Dt��Dux�Du�\Dv\Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz�\D{x�D{��D|x�D|�\D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��HD�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�yHD��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�9HD�|{D��{D��HD�9HD�|{D��HD��{D�<{D�|{D��{D��{D�?�D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D���D��{D�<{D�|{D��HD��{D�<{D�|{D��{D��{D�<{D�|{D���D��{D�<{D�|{D��{D��{D�<{D�|{D���D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��HD�9HD�|{D���D��{D�<{D�|{D��{D��{D�9HD�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��HD�<{D�|{D��{D��{D�<{D��D��{D��{D�<{D�|{D��{D��{D�?�D�|{D��{D��{D�9HD�|{D���D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D���D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��HD�<{D�|{D���D���D�<{D�|{D��{D��{D�<{D�yHD��HD��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�<{D�|{D��{D��{D�<{D�|{D��{D��HD�9HD�yHD��{D��{D�<{D�|{D���D��{D�<{D�|{D��{D���D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�9HD�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dÿ�D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��HD�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D��Dм{D��{D�<{D�|{DѼ{D��{D�?�D�|{DҼ{D��{D�?�D�|{DӼ{D��{D�9HD�|{DԼ{D��{D�<{D�|{Dտ�D��{D�<{D�|{DֹHD��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��HD�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D��D修D��{D�<{D�|{D�{D��{D�9HD�yHD�{D���D�<{D�|{D�{D��{D�?�D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�yHD�{D��HD�9HD�yHD�{D��{D�<{D�|{D��{D��{D�?�D�|{D��HD��HD�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��HD�9HD�yHD��{D��{D�<{D�\{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�$A�A��UA�v�A�+A�
	A��YA�&A��jA���A�F?A��sA�͟A���A��:A��4A���A���A���A��=A��A�^A�=A��A�%A���A�ĜA���A��iA�~A���A���A�3hA��4A���A��,A��bA�A��6A�3�A��UA���A�՛A�Q�A�1�A���A��A�d&A���A�e�A�Y�A�E�A��A���A��A��A�^A�HKA�uA���A�R�A��gA�.IA�fA�ffA���A���A��TA�|A��A��A��_A��A��%A��-A��cA�K�A�0�A�v�A�ΥA��_A��MA�~�A��A�xA��A��+A���A�Z�A���A�O�A��|A�e�A��#A�aHA�'A|��Az�$Ay#�Aw+Au�AudZAt�At/�AriDAp\�Ao�IAo�AnVAm^5Al�Al��Ak~(Ai�Ag�/Agi�Ag)�Af�Ae��Adw2Ad�Ac��Ac��Ac�Aa�AaiDA`��A`��A`A�A_�[A_c A_/�A^�HA]�A]v�A\%FAZȴAZ!-AYAW�	AVv`AU�<AT��AT4AR��AQ�6AP~�AO�"AN��AM�]AK��AI�9AI+kAH�'AG�zAF�rAE4AD��AD1'AB��A@j�A?�fA>B�A=E�A=JA<�A:��A:iDA9�A9
=A8m�A7��A7V�A6^5A5�
A54�A4��A4MA3�fA3q�A3A2�A2�A1��A0�
A/bNA.��A.��A.;�A-�*A,��A,;A+��A+TaA*�3A*MjA)��A(y�A'�A'��A'JA%��A$�A#v`A!�DA!B�A ��A q�A�A�XAJ�A�8A��A��A�~A��A�A��A�[A~�A_A5�A�A�)A��A8�A��A�6AZ�A�}A� A�.AsA��A�A��Aw2A/�A��A6A� A��A��A{JAe,AQ�A!�Am�A
�	A�AA A�AAn/Ah
AMA�FA�0AffA!�A �@�Q@���@��\@��u@�0�@��D@��@�:@�G@�4@�a�@�S@�@��@�P@�$@��@�v�@��Q@�.I@�YK@�IR@ܵ�@�U�@��@ת�@���@��@�)�@Ҍ�@�c@��@�>B@�>�@��@�I�@��@�˒@�y�@�@�e@��v@���@��@��@��0@��~@���@�M�@�s@��&@��N@�s�@�G@�0�@��8@���@�  @���@�C-@�33@��f@�8�@�� @���@�|@���@�W�@��@���@��@�ԕ@��$@��.@��m@���@��@�qv@�/�@���@��'@���@�s�@�D�@�@�7�@��@�t�@��u@�_@�	�@���@�4n@�  @��d@��M@�Ɇ@�J#@��A@���@��@��@�ں@�Ĝ@���@���@�Q�@�H@�1�@��@�
�@�	@�� @���@��@�H@��"@�+@��M@�Ta@��g@���@�x@�G�@��@��5@���@�8�@���@�a@��@���@��U@���@��o@���@��K@��@���@�dZ@�\�@�O�@�(�@�(�@�>�@��f@��\@�PH@��A@�a�@���@�($@��@��q@���@���@��+@�bN@��&@�\)@�Y@�
=@�S@��@��2@���@�H�@�@���@��:@�f�@�S�@�A @��@��@��D@�U2@�K^@�+k@�x@�@�@�u@���@���@�F�@��@�p;@�Ov@��@�ݘ@��w@��P@�O@�G�@��@���@�b@���@���@�j�@�A�@��@��5@���@�U2@��@�@�Q@n/@~�\@~GE@~
�@}�M@}Dg@}0�@}V@|e�@{RT@z�B@zYK@y�@y#�@xh�@w��@w\)@v�H@v�r@v($@u�^@uT�@t�z@toi@t�e@u%@u^�@u}�@u�@um]@u2a@tw�@tK^@t �@s�@@r�@qhs@q \@qDg@qG�@qJ�@q?}@p�K@pq@poi@p1'@o��@o��@o4�@n�B@nQ@n8�@m��@l�5@lS�@l9X@k�@k�{@k)_@k�@j��@j�"@j��@jTa@j#:@i��@i�n@i^�@i�@h�$@g�$@f�]@f{�@fQ@fM�@f@e@e}�@e[W@eG�@e7L@dN�@c�@cy�@c=@b�c@b�@b_�@b.�@b �@aϫ@a��@a/@`�/@`��@`�@`�o@`r�@`I�@`4n@_�W@_ݘ@_�@_��@_Mj@_;d@_�@_�@^�s@^\�@]O�@\��@\:�@[��@[C�@Z�L@Z5?@Y��@Y��@Y��@Y@X֡@X�@XbN@W�*@V��@V��@V��@Vn�@V=q@U��@U��@T��@T�Y@T[�@Sƨ@SW?@Rں@R�1@R@�@Q�N@QX@Q#�@PɆ@P��@P`�@O�a@O+@N��@N�r@N=q@Mu�@M�@L��@L�e@LD�@K�@K�0@K�f@KK�@KK�@K8@K1�@K"�@J��@J͟@J
�@IA @I�@H��@H�@H��@H��@HM@H�@G�@F��@Fa|@F�@E��@E�#@E��@Ee,@E:�@D�U@DQ�@C�]@C�[@C��@CH�@B�'@B��@Ba|@BC�@B-@A�#@Ax�@ADg@A�@@�)@@j@@S�@@S�@@S�@@H@@7�@?�@?{J@?�@?~�@?.I@>��@>�@>YK@=�o@=��@=rG@=@<��@<h�@<�@;��@;P�@:�@:�m@:~�@9�@9hs@9N<@98�@9@@8�@8�I@7�@7�K@7s@7dZ@7RT@7�@6ȴ@6M�@5ϫ@5��@5o @5f�@5^�@5T�@5O�@5�@4oi@44n@4/�@4-�@4�@4�@3b�@2��@2� @2p;@1�>@1�~@1X@1Dg@1(�@0��@0y>@0"h@/��@/dZ@/@O@/�@/ i@.�@.�+@.@-�H@-��@-+@,�f@,��@,��@,�$@,�9@,�@,�e@,�z@,��@,z�@,h�@,U2@,C-@,�@+��@*҉@*a|@*4@)�@)��@)[W@)+@(��@(�@(Ft@'��@'�V@'9�@'C@&��@&�B@&��@&�@&��@&Z�@&!�@%��@%��@%��@%�h@%zx@%@$�@$��@$z�@$N�@$7@#��@#�[@#j�@#'�@#�@#
=@"�@"�B@"��@"�<@"��@"_�@"($@"�@!��@!�@!X@!N<@!F@!Dg@!7L@ �E@ ]d@ ~@ݘ@�V@�$@�P@U�@�@��@�r@p;@Q@@�)@@j@+�@��@�[@��@oi@�@��@�r@�@�}@;d@�@�c@�@�@��@��@�+@3�@@�@J@�@�@|@:�@q@%@��@�5@��@ѷ@��@�_@`�@�Q@��@e�@!-@@��@��@��@��@}V@s�@v�@Z�@4@��@
�@�@��@N<@7L@�|@�@q@N�@'R@b@�r@�@��@�@�*@��@g�@�@�,@�@ff@�@��@w2@a�@S&@&�@�	@��@Ft@�+@��@��@Mj@��@ff@ �@�C@T�@ \@�@+@@��@�@ѷ@�I@bN@Ft@7@��@�6@�@6z@
=@
�M@
�H@
�@
p;@	��@	��@	a�@	+�@	@�?@��@��@�@��@��@�D@�@m�@>B@�r@��@��@Mj@$t@�@��@�x@c @.�@�@��@�j@��@�~@p�@S&@*0@�|@�p@_@9X@,=@~@�
@�@@�P@g�@a@_p@RT@H�@"�@�@��@s�@ff@Q@E�@�@�@ϫ@�d@��@j@�@ ��@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�$A�A��UA�v�A�+A�
	A��YA�&A��jA���A�F?A��sA�͟A���A��:A��4A���A���A���A��=A��A�^A�=A��A�%A���A�ĜA���A��iA�~A���A���A�3hA��4A���A��,A��bA�A��6A�3�A��UA���A�՛A�Q�A�1�A���A��A�d&A���A�e�A�Y�A�E�A��A���A��A��A�^A�HKA�uA���A�R�A��gA�.IA�fA�ffA���A���A��TA�|A��A��A��_A��A��%A��-A��cA�K�A�0�A�v�A�ΥA��_A��MA�~�A��A�xA��A��+A���A�Z�A���A�O�A��|A�e�A��#A�aHA�'A|��Az�$Ay#�Aw+Au�AudZAt�At/�AriDAp\�Ao�IAo�AnVAm^5Al�Al��Ak~(Ai�Ag�/Agi�Ag)�Af�Ae��Adw2Ad�Ac��Ac��Ac�Aa�AaiDA`��A`��A`A�A_�[A_c A_/�A^�HA]�A]v�A\%FAZȴAZ!-AYAW�	AVv`AU�<AT��AT4AR��AQ�6AP~�AO�"AN��AM�]AK��AI�9AI+kAH�'AG�zAF�rAE4AD��AD1'AB��A@j�A?�fA>B�A=E�A=JA<�A:��A:iDA9�A9
=A8m�A7��A7V�A6^5A5�
A54�A4��A4MA3�fA3q�A3A2�A2�A1��A0�
A/bNA.��A.��A.;�A-�*A,��A,;A+��A+TaA*�3A*MjA)��A(y�A'�A'��A'JA%��A$�A#v`A!�DA!B�A ��A q�A�A�XAJ�A�8A��A��A�~A��A�A��A�[A~�A_A5�A�A�)A��A8�A��A�6AZ�A�}A� A�.AsA��A�A��Aw2A/�A��A6A� A��A��A{JAe,AQ�A!�Am�A
�	A�AA A�AAn/Ah
AMA�FA�0AffA!�A �@�Q@���@��\@��u@�0�@��D@��@�:@�G@�4@�a�@�S@�@��@�P@�$@��@�v�@��Q@�.I@�YK@�IR@ܵ�@�U�@��@ת�@���@��@�)�@Ҍ�@�c@��@�>B@�>�@��@�I�@��@�˒@�y�@�@�e@��v@���@��@��@��0@��~@���@�M�@�s@��&@��N@�s�@�G@�0�@��8@���@�  @���@�C-@�33@��f@�8�@�� @���@�|@���@�W�@��@���@��@�ԕ@��$@��.@��m@���@��@�qv@�/�@���@��'@���@�s�@�D�@�@�7�@��@�t�@��u@�_@�	�@���@�4n@�  @��d@��M@�Ɇ@�J#@��A@���@��@��@�ں@�Ĝ@���@���@�Q�@�H@�1�@��@�
�@�	@�� @���@��@�H@��"@�+@��M@�Ta@��g@���@�x@�G�@��@��5@���@�8�@���@�a@��@���@��U@���@��o@���@��K@��@���@�dZ@�\�@�O�@�(�@�(�@�>�@��f@��\@�PH@��A@�a�@���@�($@��@��q@���@���@��+@�bN@��&@�\)@�Y@�
=@�S@��@��2@���@�H�@�@���@��:@�f�@�S�@�A @��@��@��D@�U2@�K^@�+k@�x@�@�@�u@���@���@�F�@��@�p;@�Ov@��@�ݘ@��w@��P@�O@�G�@��@���@�b@���@���@�j�@�A�@��@��5@���@�U2@��@�@�Q@n/@~�\@~GE@~
�@}�M@}Dg@}0�@}V@|e�@{RT@z�B@zYK@y�@y#�@xh�@w��@w\)@v�H@v�r@v($@u�^@uT�@t�z@toi@t�e@u%@u^�@u}�@u�@um]@u2a@tw�@tK^@t �@s�@@r�@qhs@q \@qDg@qG�@qJ�@q?}@p�K@pq@poi@p1'@o��@o��@o4�@n�B@nQ@n8�@m��@l�5@lS�@l9X@k�@k�{@k)_@k�@j��@j�"@j��@jTa@j#:@i��@i�n@i^�@i�@h�$@g�$@f�]@f{�@fQ@fM�@f@e@e}�@e[W@eG�@e7L@dN�@c�@cy�@c=@b�c@b�@b_�@b.�@b �@aϫ@a��@a/@`�/@`��@`�@`�o@`r�@`I�@`4n@_�W@_ݘ@_�@_��@_Mj@_;d@_�@_�@^�s@^\�@]O�@\��@\:�@[��@[C�@Z�L@Z5?@Y��@Y��@Y��@Y@X֡@X�@XbN@W�*@V��@V��@V��@Vn�@V=q@U��@U��@T��@T�Y@T[�@Sƨ@SW?@Rں@R�1@R@�@Q�N@QX@Q#�@PɆ@P��@P`�@O�a@O+@N��@N�r@N=q@Mu�@M�@L��@L�e@LD�@K�@K�0@K�f@KK�@KK�@K8@K1�@K"�@J��@J͟@J
�@IA @I�@H��@H�@H��@H��@HM@H�@G�@F��@Fa|@F�@E��@E�#@E��@Ee,@E:�@D�U@DQ�@C�]@C�[@C��@CH�@B�'@B��@Ba|@BC�@B-@A�#@Ax�@ADg@A�@@�)@@j@@S�@@S�@@S�@@H@@7�@?�@?{J@?�@?~�@?.I@>��@>�@>YK@=�o@=��@=rG@=@<��@<h�@<�@;��@;P�@:�@:�m@:~�@9�@9hs@9N<@98�@9@@8�@8�I@7�@7�K@7s@7dZ@7RT@7�@6ȴ@6M�@5ϫ@5��@5o @5f�@5^�@5T�@5O�@5�@4oi@44n@4/�@4-�@4�@4�@3b�@2��@2� @2p;@1�>@1�~@1X@1Dg@1(�@0��@0y>@0"h@/��@/dZ@/@O@/�@/ i@.�@.�+@.@-�H@-��@-+@,�f@,��@,��@,�$@,�9@,�@,�e@,�z@,��@,z�@,h�@,U2@,C-@,�@+��@*҉@*a|@*4@)�@)��@)[W@)+@(��@(�@(Ft@'��@'�V@'9�@'C@&��@&�B@&��@&�@&��@&Z�@&!�@%��@%��@%��@%�h@%zx@%@$�@$��@$z�@$N�@$7@#��@#�[@#j�@#'�@#�@#
=@"�@"�B@"��@"�<@"��@"_�@"($@"�@!��@!�@!X@!N<@!F@!Dg@!7L@ �E@ ]d@ ~@ݘ@�V@�$@�P@U�@�@��@�r@p;@Q@@�)@@j@+�@��@�[@��@oi@�@��@�r@�@�}@;d@�@�c@�@�@��@��@�+@3�@@�@J@�@�@|@:�@q@%@��@�5@��@ѷ@��@�_@`�@�Q@��@e�@!-@@��@��@��@��@}V@s�@v�@Z�@4@��@
�@�@��@N<@7L@�|@�@q@N�@'R@b@�r@�@��@�@�*@��@g�@�@�,@�@ff@�@��@w2@a�@S&@&�@�	@��@Ft@�+@��@��@Mj@��@ff@ �@�C@T�@ \@�@+@@��@�@ѷ@�I@bN@Ft@7@��@�6@�@6z@
=@
�M@
�H@
�@
p;@	��@	��@	a�@	+�@	@�?@��@��@�@��@��@�D@�@m�@>B@�r@��@��@Mj@$t@�@��@�x@c @.�@�@��@�j@��@�~@p�@S&@*0@�|@�p@_@9X@,=@~@�
@�@@�P@g�@a@_p@RT@H�@"�@�@��@s�@ff@Q@E�@�@�@ϫ@�d@��@j@�@ ��@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�QB��B��B�8B�
B��B��B�B�nB� B��B�4B��B��B��B�ZB�@B��B��B��B�FB�zB�ZB��B�\B��B�IB��B�B��B�=B|�BsB`�BZ7BVBU�BT�BZ�B\]B]/BS�BMjB:�B+6B(�B$�B#:BCBBKBYBB_B�vB�?B�MB��B��B޸B�SB˒B��B�B��B�_B~]BoBc�BX+BB[B1�B�B��B�$B�B�WB��B��B�nB��Bv`Bc�BN"BE�B>B=VB1�B)*B�BYB�B
XBB
�DB
�*B
��B
˒B
ÖB
��B
��B
��B
��B
��B
� B
�?B
��B
�:B
��B
�fB
��B
��B
~B
w2B
l�B
h�B
f�B
c�B
_�B
Z�B
W?B
V�B
UMB
S�B
O�B
LdB
K)B
H1B
GB
D�B
A�B
AUB
?}B
;�B
9$B
5tB
./B
)yB
"�B
�B
�B
�B
	�B
+B
�B	��B	�[B	�IB	�B	�eB	�B	��B	οB	̳B	�B	�YB	��B	�wB	��B	�B	�)B	��B	��B	��B	�B	�B	�mB	�{B	��B	��B	�PB	�DB	��B	��B	�'B	��B	cB	{�B	z�B	x�B	wLB	u?B	s3B	o�B	n/B	iDB	fLB	d�B	b�B	`�B	^B	[#B	ZkB	X+B	VmB	S�B	Q�B	OvB	K)B	H�B	F�B	B�B	<PB	:�B	5�B	2�B	0UB	/iB	+kB	$�B	"4B	�B	�B	�B	_B	�B	 B	jB	^B		�B		7B		B	fB	_B	�B	B	B	�B	MB	�B	�B	B	B��B�BB��B��B�<B��B��B��B�jB�RB��B��B��B�%B��B��B�B�B��B�B�BܒB�QB��B�sB�B�aBӏB��B͟BʌB�B��B�3BāB��B��B�}B��B�B�B�DB�B�>B��B�8B��B�LB��B��B��B�B��B�3B��B��B�hB�B��B�aB�GB�hB�B��B�B��B��B�B��B�FB��B��B��B�8B�lB�	B��B��B�]B��B�iB��B�oB�;B�uB�mBʦB�0B�jBϑB�\B�\B��B� BѝB�4B�MBٴB��B��B�B�B�B��B��B��B�	B��B�xB��B�B��B	 OB	�B	.B	�B	�B	�B	#B	 B	!HB	!�B	"�B	&B	,=B	0B	5�B	:B	:�B	;�B	<�B	=B	=�B	@�B	B�B	EB	GEB	G�B	GzB	I�B	K�B	N�B	Q�B	RB	UgB	VmB	Z�B	\�B	]�B	^�B	_�B	`�B	abB	b�B	d�B	gmB	jB	nIB	n�B	o�B	t9B	w�B	xB	}<B	��B	��B	�YB	�YB	��B	�vB	�B	�	B	��B	�B	��B	��B	�B	�QB	��B	��B	��B	�tB	��B	�fB	��B	��B	��B	��B	�3B	�MB	��B	�B	�?B	�B	�rB	�jB	�B	�vB	�.B	�NB	�B	�,B	�mB	׍B	רB	�_B	�B	�B	�B	�B	��B	��B	ݲB	��B	�B	�FB	�B	�B	��B	�KB	�B	�B	��B	�TB	�RB	�JB	�"B	��B	�HB
 �B
B
B
�B
�B
	�B

#B
�B
�B
�B
hB
�B
�B
�B
MB
+B
)B
B
�B
!�B
$ZB
'�B
*0B
,�B
.�B
0UB
2-B
4B
6B
;B
?�B
@�B
B�B
D3B
D�B
F�B
IlB
K�B
O(B
OBB
P}B
SuB
W?B
W�B
YB
[�B
\B
[�B
\�B
]�B
^�B
^OB
^OB
^B
^OB
^�B
_pB
`BB
`�B
bNB
c�B
d�B
d�B
e�B
e�B
f2B
f�B
f�B
f�B
f�B
g�B
g�B
h>B
h�B
i*B
i*B
i*B
jKB
lB
nB
p�B
q�B
r�B
tnB
u�B
vB
vB
vB
y$B
zDB
zDB
z�B
{B
|PB
|�B
}qB
}�B
~wB
~�B
��B
�B
��B
�aB
��B
��B
��B
�B
�YB
�tB
�_B
��B
��B
��B
��B
�	B
��B
��B
��B
�vB
��B
�hB
�oB
��B
��B
�gB
��B
�mB
��B
�1B
��B
��B
�)B
�B
��B
�pB
��B
��B
��B
�:B
�,B
�,B
��B
��B
�B
�6B
��B
��B
��B
�OB
��B
��B
�'B
�vB
�B
�TB
��B
��B
�zB
�lB
�>B
��B
��B
��B
�dB
��B
�PB
��B
��B
��B
��B
��B
�VB
�qB
��B
�;B
��B
��B
�B
�[B
��B
ÖB
��B
�3B
ǔB
�B
ɺB
��B
�=B
ʌB
�)B
�xB
̘B
�PB
�B
ΥB
ΥB
�(B
ϑB
��B
��B
�HB
�HB
� B
уB
ѷB
��B
��B
�TB
�TB
�TB
�TB
�TB
�oB
�uB
өB
�uB
ӏB
�,B
�{B
�B
��B
֡B
�$B
�sB
�B
خB
�KB
��B
�QB
��B
یB
��B
�CB
��B
ބB
޸B
��B
�!B
�pB
��B
��B
��B
�|B
�B
�B
��B
�B
� B
�B
�ZB
�B
�B
�B
��B
�B
�FB
�2B
�B
�B
�B
��B
��B
�XB
�yB
�B
��B
�B
�6B
�kB
�B
�B
�=B
�B
�CB
��B
�}B
�B
� B
�B
�5B
��B
�B
�B
�[B
�GB
�GB
�B
�B
��B
��B
��B
��B
��B
�3B
�MB
�hB
�hB
�B
�B
�B
��B
�`B
��B
�2B
��B
�B
�RB
��B
�$B
�rB
�DB
�^B
��B
�0B
�dB
��B
��B
��B
��B
�PB
��B
��B
�<B
��B
��B
��B
�]B
�wB
��B
�HB
��B
��B iB �B �BUBoBoB�B�B�B�B�BuB�BB-B�B3BMBgB3BMB�BmB�B?B�B�B�BB_B�BfB�B�B	B	7B	�B
	B
XB
�B
�BB�B0B0BBBdBPB�B�B�B�B�BBB�B�B�B�B�BBBvB�BBHBHBbB}B}B�B�B B�BBTB�B�B@B[BuBuB�B�B�B�BBFB�B�B�B�BBgB�BBmB�B�B�B�B?BsB�B�B+BKBKBB�BQB�B�B�B	B=BWB�B]B�B�B/B�B�B!BpB B vB �B �B �B �B �B!B!B!|B!�B!�B"4B"4B"hB#B#TB#�B#�B#�B$@B$�B%`B%�B%�B&2B&�B&�B'B'B'8B'RB'8B'8B'mB'mB'�B($B(sB(�B)B)_B)yB)�B)�B*B*�B*�B+B+QB+�B+�B+�B,"B,qB,�B,�B-�B-�B-�B-�B.IB.�B.�B/B/B/B/B/5B/iB/�B0oB0UB0oB0�B0�B0�B1'B1AB1'B1[B1�B2B2-B1�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B�QB��B��B�8B�
B��B��B�B�nB� B��B�4B��B��B��B�ZB�@B��B��B��B�FB�zB�ZB��B�\B��B�IB��B�B��B�=B|�BsB`�BZ7BVBU�BT�BZ�B\]B]/BS�BMjB:�B+6B(�B$�B#:BCBBKBYBB_B�vB�?B�MB��B��B޸B�SB˒B��B�B��B�_B~]BoBc�BX+BB[B1�B�B��B�$B�B�WB��B��B�nB��Bv`Bc�BN"BE�B>B=VB1�B)*B�BYB�B
XBB
�DB
�*B
��B
˒B
ÖB
��B
��B
��B
��B
��B
� B
�?B
��B
�:B
��B
�fB
��B
��B
~B
w2B
l�B
h�B
f�B
c�B
_�B
Z�B
W?B
V�B
UMB
S�B
O�B
LdB
K)B
H1B
GB
D�B
A�B
AUB
?}B
;�B
9$B
5tB
./B
)yB
"�B
�B
�B
�B
	�B
+B
�B	��B	�[B	�IB	�B	�eB	�B	��B	οB	̳B	�B	�YB	��B	�wB	��B	�B	�)B	��B	��B	��B	�B	�B	�mB	�{B	��B	��B	�PB	�DB	��B	��B	�'B	��B	cB	{�B	z�B	x�B	wLB	u?B	s3B	o�B	n/B	iDB	fLB	d�B	b�B	`�B	^B	[#B	ZkB	X+B	VmB	S�B	Q�B	OvB	K)B	H�B	F�B	B�B	<PB	:�B	5�B	2�B	0UB	/iB	+kB	$�B	"4B	�B	�B	�B	_B	�B	 B	jB	^B		�B		7B		B	fB	_B	�B	B	B	�B	MB	�B	�B	B	B��B�BB��B��B�<B��B��B��B�jB�RB��B��B��B�%B��B��B�B�B��B�B�BܒB�QB��B�sB�B�aBӏB��B͟BʌB�B��B�3BāB��B��B�}B��B�B�B�DB�B�>B��B�8B��B�LB��B��B��B�B��B�3B��B��B�hB�B��B�aB�GB�hB�B��B�B��B��B�B��B�FB��B��B��B�8B�lB�	B��B��B�]B��B�iB��B�oB�;B�uB�mBʦB�0B�jBϑB�\B�\B��B� BѝB�4B�MBٴB��B��B�B�B�B��B��B��B�	B��B�xB��B�B��B	 OB	�B	.B	�B	�B	�B	#B	 B	!HB	!�B	"�B	&B	,=B	0B	5�B	:B	:�B	;�B	<�B	=B	=�B	@�B	B�B	EB	GEB	G�B	GzB	I�B	K�B	N�B	Q�B	RB	UgB	VmB	Z�B	\�B	]�B	^�B	_�B	`�B	abB	b�B	d�B	gmB	jB	nIB	n�B	o�B	t9B	w�B	xB	}<B	��B	��B	�YB	�YB	��B	�vB	�B	�	B	��B	�B	��B	��B	�B	�QB	��B	��B	��B	�tB	��B	�fB	��B	��B	��B	��B	�3B	�MB	��B	�B	�?B	�B	�rB	�jB	�B	�vB	�.B	�NB	�B	�,B	�mB	׍B	רB	�_B	�B	�B	�B	�B	��B	��B	ݲB	��B	�B	�FB	�B	�B	��B	�KB	�B	�B	��B	�TB	�RB	�JB	�"B	��B	�HB
 �B
B
B
�B
�B
	�B

#B
�B
�B
�B
hB
�B
�B
�B
MB
+B
)B
B
�B
!�B
$ZB
'�B
*0B
,�B
.�B
0UB
2-B
4B
6B
;B
?�B
@�B
B�B
D3B
D�B
F�B
IlB
K�B
O(B
OBB
P}B
SuB
W?B
W�B
YB
[�B
\B
[�B
\�B
]�B
^�B
^OB
^OB
^B
^OB
^�B
_pB
`BB
`�B
bNB
c�B
d�B
d�B
e�B
e�B
f2B
f�B
f�B
f�B
f�B
g�B
g�B
h>B
h�B
i*B
i*B
i*B
jKB
lB
nB
p�B
q�B
r�B
tnB
u�B
vB
vB
vB
y$B
zDB
zDB
z�B
{B
|PB
|�B
}qB
}�B
~wB
~�B
��B
�B
��B
�aB
��B
��B
��B
�B
�YB
�tB
�_B
��B
��B
��B
��B
�	B
��B
��B
��B
�vB
��B
�hB
�oB
��B
��B
�gB
��B
�mB
��B
�1B
��B
��B
�)B
�B
��B
�pB
��B
��B
��B
�:B
�,B
�,B
��B
��B
�B
�6B
��B
��B
��B
�OB
��B
��B
�'B
�vB
�B
�TB
��B
��B
�zB
�lB
�>B
��B
��B
��B
�dB
��B
�PB
��B
��B
��B
��B
��B
�VB
�qB
��B
�;B
��B
��B
�B
�[B
��B
ÖB
��B
�3B
ǔB
�B
ɺB
��B
�=B
ʌB
�)B
�xB
̘B
�PB
�B
ΥB
ΥB
�(B
ϑB
��B
��B
�HB
�HB
� B
уB
ѷB
��B
��B
�TB
�TB
�TB
�TB
�TB
�oB
�uB
өB
�uB
ӏB
�,B
�{B
�B
��B
֡B
�$B
�sB
�B
خB
�KB
��B
�QB
��B
یB
��B
�CB
��B
ބB
޸B
��B
�!B
�pB
��B
��B
��B
�|B
�B
�B
��B
�B
� B
�B
�ZB
�B
�B
�B
��B
�B
�FB
�2B
�B
�B
�B
��B
��B
�XB
�yB
�B
��B
�B
�6B
�kB
�B
�B
�=B
�B
�CB
��B
�}B
�B
� B
�B
�5B
��B
�B
�B
�[B
�GB
�GB
�B
�B
��B
��B
��B
��B
��B
�3B
�MB
�hB
�hB
�B
�B
�B
��B
�`B
��B
�2B
��B
�B
�RB
��B
�$B
�rB
�DB
�^B
��B
�0B
�dB
��B
��B
��B
��B
�PB
��B
��B
�<B
��B
��B
��B
�]B
�wB
��B
�HB
��B
��B iB �B �BUBoBoB�B�B�B�B�BuB�BB-B�B3BMBgB3BMB�BmB�B?B�B�B�BB_B�BfB�B�B	B	7B	�B
	B
XB
�B
�BB�B0B0BBBdBPB�B�B�B�B�BBB�B�B�B�B�BBBvB�BBHBHBbB}B}B�B�B B�BBTB�B�B@B[BuBuB�B�B�B�BBFB�B�B�B�BBgB�BBmB�B�B�B�B?BsB�B�B+BKBKBB�BQB�B�B�B	B=BWB�B]B�B�B/B�B�B!BpB B vB �B �B �B �B �B!B!B!|B!�B!�B"4B"4B"hB#B#TB#�B#�B#�B$@B$�B%`B%�B%�B&2B&�B&�B'B'B'8B'RB'8B'8B'mB'mB'�B($B(sB(�B)B)_B)yB)�B)�B*B*�B*�B+B+QB+�B+�B+�B,"B,qB,�B,�B-�B-�B-�B-�B.IB.�B.�B/B/B/B/B/5B/iB/�B0oB0UB0oB0�B0�B0�B1'B1AB1'B1[B1�B2B2-B1�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230502094216  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230502094217  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230502094218  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230502094218                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230502094218  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230502094218  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230502095901                      G�O�G�O�G�O�                
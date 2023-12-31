CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-28T11:01:22Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  `   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ox   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ۈ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۸   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ޸   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20200628110122  20200628110122  5906156 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               ,A   AO  7912                            2B  A   NAVIS_A                         1020                            170425                          863 @�$�F�ܤ1   @�$���	@7s�E����ck�
=p�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      ,A   A   A   @�  @���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D��3D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�z�@�G�@�G�A=qA:=qAZ=qAz=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B&�\B.�\B6�\B>�\BF�\BN�\BV�\B^�\Bf�\Bn�\Bv�\B~�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI�qCK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�޹C���C���C���C�޹C���C���C���C���D h�D ��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��D	h�D	��D
h�D
��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D�Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��D h�D ��D!h�D!��D"h�D"��D#h�D#��D$h�D$��D%h�D%��D&h�D&��D'h�D'��D(h�D(��D)h�D)��D*h�D*��D+h�D+��D,h�D,��D-h�D-��D.h�D.��D/h�D/��D0h�D0��D1h�D1��D2h�D2��D3h�D3��D4h�D4��D5h�D5��D6h�D6��D7h�D7��D8h�D8��D9h�D9��D:h�D:��D;h�D;��D<h�D<��D=h�D=��D>h�D>��D?h�D?��D@h�D@��DAh�DA��DBh�DB��DCh�DC��DDh�DD��DEh�DE��DFh�DF��DGh�DG��DHh�DH��DIh�DI��DJh�DJ��DKh�DK��DLh�DL��DMh�DM��DNh�DN��DOh�DO��DPh�DP��DQh�DQ��DRh�DR��DSh�DS��DTh�DT��DUh�DU��DVh�DV��DWh�DW��DXh�DX��DYh�DY��DZh�DZ��D[h�D[��D\h�D\��D]h�D]��D^h�D^��D_h�D_��D`h�D`��Dah�Da��Dbh�Db��Dch�Dc��Ddh�Dd��Deh�De��Dfh�Df��Dgh�Dg��Dhh�Dh��Dih�Di��Djh�Dj��Dkh�Dk��Dlh�Dl��Dmh�Dm��Dnh�Dn��Doh�Do��Dph�Dp��Dqh�Dq��Drh�Dr��Dsh�Ds��Dth�Dt��Duh�Du��Dvh�Dv��Dwh�Dw��Dxh�Dx��Dyh�Dy��Dzh�Dz��D{h�D{��D|h�D|��D}h�D}��D~h�D~��Dh�D��D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D´{D��{D�4{D�t{Dô{D��{D�4{D�t{DĴ{D��{D�4{D�t{DŴ{D��{D�4{D�t{Dƴ{D��{D�4{D�t{DǴ{D��{D�4{D�t{Dȴ{D��{D�4{D�t{Dɴ{D��{D�4{D�t{Dʴ{D��{D�4{D�t{D˴{D��{D�4{D�t{D̴{D��{D�4{D�t{Dʹ{D��{D�4{D�t{Dδ{D��{D�4{D�t{Dϴ{D��{D�4{D�t{Dд{D��{D�4{D�t{DѴ{D��{D�4{D�t{DҴ{D��{D�4{D�t{DӴ{D��{D�4{D�t{DԴ{D��{D�4{D�t{Dմ{D��{D�4{D�t{Dִ{D��{D�4{D�t{D״{D��{D�4{D�t{Dش{D��{D�4{D�t{Dٴ{D��{D�4{D�t{Dڴ{D��{D�4{D�t{D۴{D��{D�4{D�t{Dܷ�D��{D�4{D�t{Dݴ{D��{D�4{D�t{D޴{D��{D�4{D�t{Dߴ{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D���D�4{D�t{D��{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D���D�.1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�/A�&�A��Aΰ!A�O�A��`A�$�A̕�A�M�A�oA�oA��A�oA�ȴA�v�A�l�A�l�A�Q�A�&�A��A�M�Aɩ�A�;dA��#A�;dA���A��mA��^A�=qA��-A���A��A��A��A�(�A��`A�n�A�  A�M�A��/A���A�ZA�bA���A�A�ĜA�"�A��!A�  A�r�A�ƨA��#A���A�XA�|�A���A�A�A���A�`BA�A���A�ƨA�dZA��A�r�A�A�p�A�-A�33A� �A�-A�  A��A��uA�$�A�A�A��hA�XA��mA�S�A���A�  A�  A���A�I�A��A��FA��
A���A��A�1'A���A��jA��A��A��RA��^A�1'A��PA�9XA�O�A��A��DA���A�A|ȴAydZAw
=Au�FAs�7Ap$�Ak�Ai
=Ahv�Agl�Af9XAet�Ac�A_�mA]/AY�;AW`BAV�\AT��ARz�AP�jAN�\AL1'AK�AK��AK33AI%AF�AC��AB�AA\)A?�^A>�A<�`A<E�A;�A;VA9?}A7�A6bA5��A4��A4(�A3+A1��A/�A.��A.ffA.�A-A+ƨA*��A*{A)�
A)��A(�9A&r�A$�\A#;dA"5?A!��A!t�A 1'A?}AffAAS�A�PA
=A�A�AK�A�!A�
AC�A��A5?A��A�;A$�A{AC�A%A�+AhsA
��A	�A�RAƨAjAG�A��A33A�PA ��A �DA @�M�@�Ĝ@�(�@��H@��-@��7@��@���@�b@�+@��7@�l�@@�@�?}@��/@�1'@�+@�X@��
@�R@�7L@�A�@�@��@��#@�I�@��@�@�&�@�+@ٙ�@�Z@ץ�@�"�@�Ĝ@�~�@�M�@���@�/@мj@�r�@϶F@�K�@�+@�ȴ@�@�A�@ʇ+@�X@�9X@��@Ə\@�o@Ə\@���@��@�?}@�z�@�33@�V@�?}@���@�I�@��@�@��@���@�X@�1'@�;d@���@���@�Z@�dZ@��!@�$�@��T@�p�@�A�@���@�l�@���@���@���@���@�
=@��m@�dZ@��H@��`@�bN@��m@���@�5?@��T@�?}@��u@���@�dZ@��H@�~�@�^5@�^5@�5?@���@��/@�Q�@��@�t�@�o@��R@�n�@�5?@��-@�X@��`@�A�@��m@��@�@�ȴ@�E�@�@��9@� �@��w@�dZ@�~�@���@���@�p�@�%@�r�@�b@���@��@�+@���@�~�@�E�@�5?@�$�@���@��#@�x�@�hs@�7L@�Ĝ@�r�@�1'@��m@��@�|�@�dZ@�;d@�o@��@��@���@���@���@�M�@��@��-@��h@�hs@�?}@�&�@��@��@��@��@��@�%@��`@��`@��@�bN@�j@��@� �@��F@��;@��@�1@���@��;@��;@�ƨ@���@�l�@��y@���@�/@��9@�j@�I�@�9X@�  @���@��m@��@�C�@�M�@�@���@���@���@���@��7@��@�x�@�hs@�X@�G�@���@�(�@��@���@�\)@��@�^5@�-@�{@��@��-@���@���@��T@���@�O�@�%@��/@��j@��u@�bN@� �@;d@~�y@~ȴ@~V@~ff@~E�@}@}�h@}`B@}`B@}`B@}p�@~V@}�@}�@|�@}V@}/@}�@}V@|�j@|�j@|�D@|�D@|Z@{��@{�m@{�F@{��@{�@{dZ@{33@z�!@zn�@z^5@zM�@z=q@z-@y�^@y&�@x�`@x��@x��@w�;@x  @w�@w�;@w�@wK�@w+@w+@v��@u�-@u`B@u/@t�/@tj@s�F@r�@r~�@r~�@r-@q�@q��@q��@qhs@q7L@q�@p�9@pb@o�@o;d@n�y@n�R@n��@nE�@n$�@n@m�T@m�@m�@m@m�h@m�@m�@l�j@lj@l1@k�m@k�
@k�@kS�@k33@ko@j�@j��@j��@j~�@j=q@i�@iG�@hĜ@g�@gl�@g\)@g+@f�y@f�R@f��@f��@f��@fE�@e�-@e�@d�@dj@d9X@d�@d1@d1@c��@c�m@c�@cC�@c"�@b�@b��@bM�@b-@a��@a��@a��@a��@aX@`r�@`1'@`b@`  @_�;@_�@_l�@_;d@^�y@^��@^$�@]�-@]O�@\�@\Z@\1@[�
@[��@[�@[S�@["�@Z��@Z��@Z=q@YX@Y7L@X��@X�@XA�@W��@W+@V��@VE�@V@U@U��@UV@T�j@T�@S�
@SdZ@S@R�H@R��@R=q@Q�@Q��@Qx�@Q&�@P��@P��@P��@PQ�@O�w@N�@N�+@N{@M�T@M@Mp�@M/@L�/@LZ@L(�@K��@K�
@K��@K�@KS�@J�H@J�\@I�@I�7@Ihs@H�9@G��@Gl�@G�@F��@F�y@F�y@Fȴ@Fȴ@F�+@F$�@E`B@E�@D��@D�/@D�@D�j@D�j@D��@Dz�@C�m@CC�@C"�@B�H@B��@B�!@B�\@B�@A��@A�#@A�#@A��@A�^@A�^@A�7@Ax�@Ax�@AG�@A&�@@�`@@�u@@Q�@?�@?K�@?�@>�y@>v�@>$�@>{@>@=��@=p�@=O�@=?}@<�@<�D@;�
@;33@:��@:�\@:~�@:�@9�^@9hs@97L@9�@8�`@8�u@8Q�@8b@7��@7l�@7
=@6�y@6�@6��@6V@6{@5�T@5��@5�@5V@4��@4j@49X@4�@3�m@3��@333@2��@2=q@1�@1��@1��@1��@1��@1hs@1%@0�u@01'@/��@/�P@/;d@.�y@.��@.v�@.v�@.v�@.V@.5?@.{@-�@-@-�@-/@,�/@,��@,z�@,j@,�@+��@+t�@+@*��@*n�@*M�@*J@)��@)�7@)�7@)x�@)hs@)hs@)&�@(Ĝ@(�9@(�9@(Q�@(  @'|�@'\)@'K�@'+@'�@'�@'
=@&��@&�@&�@&�+@&E�@&5?@%�@%�T@%@%��@%O�@%V@$�@$��@$�@$Z@$I�@$9X@$(�@$1@#�
@#��@#t�@#C�@"�@"��@"�!@"~�@"^5@"-@!��@!��@!�^@!X@!%@ �`@ Q�@  �@l�@�y@v�@{@�@�T@�T@��@�h@O�@/@�@��@�@I�@1@�m@�
@��@��@t�@dZ@33@"�@��@~�@�@��@hs@G�@�@��@Ĝ@��@�u@�@ �@�@l�@\)@;d@�y@�R@V@5?@5?@5?@�@@O�@�/@��@9X@��@�m@ƨ@�@o@��@�\@n�@M�@�@�#@��@�7@X@%@bN@A�@ �@�w@�@|�@l�@\)@\)@�@ȴ@�R@�+@E�@{@�@�T@�@�@�T@��@�-@`B@�@�j@j@Z@Z@�@�
@ƨ@�F@�F@��@t�@dZ@33@@
�H@
�H@
�H@
�!@
^5@
-@
J@	�^@	��@	hs@	X@	G�@	&�@	%@�`@�9@�u@r�@1'@b@�w@�P@�P@|�@\)@;d@�@�@
=@�R@��@�+@ff@5?@{@@��@�-@�@`B@?}@�@��@�@z�@j@Z@Z@(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1'A�/A�&�A��Aΰ!A�O�A��`A�$�A̕�A�M�A�oA�oA��A�oA�ȴA�v�A�l�A�l�A�Q�A�&�A��A�M�Aɩ�A�;dA��#A�;dA���A��mA��^A�=qA��-A���A��A��A��A�(�A��`A�n�A�  A�M�A��/A���A�ZA�bA���A�A�ĜA�"�A��!A�  A�r�A�ƨA��#A���A�XA�|�A���A�A�A���A�`BA�A���A�ƨA�dZA��A�r�A�A�p�A�-A�33A� �A�-A�  A��A��uA�$�A�A�A��hA�XA��mA�S�A���A�  A�  A���A�I�A��A��FA��
A���A��A�1'A���A��jA��A��A��RA��^A�1'A��PA�9XA�O�A��A��DA���A�A|ȴAydZAw
=Au�FAs�7Ap$�Ak�Ai
=Ahv�Agl�Af9XAet�Ac�A_�mA]/AY�;AW`BAV�\AT��ARz�AP�jAN�\AL1'AK�AK��AK33AI%AF�AC��AB�AA\)A?�^A>�A<�`A<E�A;�A;VA9?}A7�A6bA5��A4��A4(�A3+A1��A/�A.��A.ffA.�A-A+ƨA*��A*{A)�
A)��A(�9A&r�A$�\A#;dA"5?A!��A!t�A 1'A?}AffAAS�A�PA
=A�A�AK�A�!A�
AC�A��A5?A��A�;A$�A{AC�A%A�+AhsA
��A	�A�RAƨAjAG�A��A33A�PA ��A �DA @�M�@�Ĝ@�(�@��H@��-@��7@��@���@�b@�+@��7@�l�@@�@�?}@��/@�1'@�+@�X@��
@�R@�7L@�A�@�@��@��#@�I�@��@�@�&�@�+@ٙ�@�Z@ץ�@�"�@�Ĝ@�~�@�M�@���@�/@мj@�r�@϶F@�K�@�+@�ȴ@�@�A�@ʇ+@�X@�9X@��@Ə\@�o@Ə\@���@��@�?}@�z�@�33@�V@�?}@���@�I�@��@�@��@���@�X@�1'@�;d@���@���@�Z@�dZ@��!@�$�@��T@�p�@�A�@���@�l�@���@���@���@���@�
=@��m@�dZ@��H@��`@�bN@��m@���@�5?@��T@�?}@��u@���@�dZ@��H@�~�@�^5@�^5@�5?@���@��/@�Q�@��@�t�@�o@��R@�n�@�5?@��-@�X@��`@�A�@��m@��@�@�ȴ@�E�@�@��9@� �@��w@�dZ@�~�@���@���@�p�@�%@�r�@�b@���@��@�+@���@�~�@�E�@�5?@�$�@���@��#@�x�@�hs@�7L@�Ĝ@�r�@�1'@��m@��@�|�@�dZ@�;d@�o@��@��@���@���@���@�M�@��@��-@��h@�hs@�?}@�&�@��@��@��@��@��@�%@��`@��`@��@�bN@�j@��@� �@��F@��;@��@�1@���@��;@��;@�ƨ@���@�l�@��y@���@�/@��9@�j@�I�@�9X@�  @���@��m@��@�C�@�M�@�@���@���@���@���@��7@��@�x�@�hs@�X@�G�@���@�(�@��@���@�\)@��@�^5@�-@�{@��@��-@���@���@��T@���@�O�@�%@��/@��j@��u@�bN@� �@;d@~�y@~ȴ@~V@~ff@~E�@}@}�h@}`B@}`B@}`B@}p�@~V@}�@}�@|�@}V@}/@}�@}V@|�j@|�j@|�D@|�D@|Z@{��@{�m@{�F@{��@{�@{dZ@{33@z�!@zn�@z^5@zM�@z=q@z-@y�^@y&�@x�`@x��@x��@w�;@x  @w�@w�;@w�@wK�@w+@w+@v��@u�-@u`B@u/@t�/@tj@s�F@r�@r~�@r~�@r-@q�@q��@q��@qhs@q7L@q�@p�9@pb@o�@o;d@n�y@n�R@n��@nE�@n$�@n@m�T@m�@m�@m@m�h@m�@m�@l�j@lj@l1@k�m@k�
@k�@kS�@k33@ko@j�@j��@j��@j~�@j=q@i�@iG�@hĜ@g�@gl�@g\)@g+@f�y@f�R@f��@f��@f��@fE�@e�-@e�@d�@dj@d9X@d�@d1@d1@c��@c�m@c�@cC�@c"�@b�@b��@bM�@b-@a��@a��@a��@a��@aX@`r�@`1'@`b@`  @_�;@_�@_l�@_;d@^�y@^��@^$�@]�-@]O�@\�@\Z@\1@[�
@[��@[�@[S�@["�@Z��@Z��@Z=q@YX@Y7L@X��@X�@XA�@W��@W+@V��@VE�@V@U@U��@UV@T�j@T�@S�
@SdZ@S@R�H@R��@R=q@Q�@Q��@Qx�@Q&�@P��@P��@P��@PQ�@O�w@N�@N�+@N{@M�T@M@Mp�@M/@L�/@LZ@L(�@K��@K�
@K��@K�@KS�@J�H@J�\@I�@I�7@Ihs@H�9@G��@Gl�@G�@F��@F�y@F�y@Fȴ@Fȴ@F�+@F$�@E`B@E�@D��@D�/@D�@D�j@D�j@D��@Dz�@C�m@CC�@C"�@B�H@B��@B�!@B�\@B�@A��@A�#@A�#@A��@A�^@A�^@A�7@Ax�@Ax�@AG�@A&�@@�`@@�u@@Q�@?�@?K�@?�@>�y@>v�@>$�@>{@>@=��@=p�@=O�@=?}@<�@<�D@;�
@;33@:��@:�\@:~�@:�@9�^@9hs@97L@9�@8�`@8�u@8Q�@8b@7��@7l�@7
=@6�y@6�@6��@6V@6{@5�T@5��@5�@5V@4��@4j@49X@4�@3�m@3��@333@2��@2=q@1�@1��@1��@1��@1��@1hs@1%@0�u@01'@/��@/�P@/;d@.�y@.��@.v�@.v�@.v�@.V@.5?@.{@-�@-@-�@-/@,�/@,��@,z�@,j@,�@+��@+t�@+@*��@*n�@*M�@*J@)��@)�7@)�7@)x�@)hs@)hs@)&�@(Ĝ@(�9@(�9@(Q�@(  @'|�@'\)@'K�@'+@'�@'�@'
=@&��@&�@&�@&�+@&E�@&5?@%�@%�T@%@%��@%O�@%V@$�@$��@$�@$Z@$I�@$9X@$(�@$1@#�
@#��@#t�@#C�@"�@"��@"�!@"~�@"^5@"-@!��@!��@!�^@!X@!%@ �`@ Q�@  �@l�@�y@v�@{@�@�T@�T@��@�h@O�@/@�@��@�@I�@1@�m@�
@��@��@t�@dZ@33@"�@��@~�@�@��@hs@G�@�@��@Ĝ@��@�u@�@ �@�@l�@\)@;d@�y@�R@V@5?@5?@5?@�@@O�@�/@��@9X@��@�m@ƨ@�@o@��@�\@n�@M�@�@�#@��@�7@X@%@bN@A�@ �@�w@�@|�@l�@\)@\)@�@ȴ@�R@�+@E�@{@�@�T@�@�@�T@��@�-@`B@�@�j@j@Z@Z@�@�
@ƨ@�F@�F@��@t�@dZ@33@@
�H@
�H@
�H@
�!@
^5@
-@
J@	�^@	��@	hs@	X@	G�@	&�@	%@�`@�9@�u@r�@1'@b@�w@�P@�P@|�@\)@;d@�@�@
=@�R@��@�+@ff@5?@{@@��@�-@�@`B@?}@�@��@�@z�@j@Z@Z@(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B!�B!�B"�B#�B.B49B5?B5?B1'B,B$�B(�B0!B5?B;dBB�BbNBs�B�=B��B�'B�
B�B��B��BÖB�?B�B�-B��BB��B�5B��BBBB��B��B��B��BB�B2-BH�B7LB/B7LB]/BdZBl�Be`BjBs�Bu�Bp�B|�B{�By�Bx�Bz�B}�B{�Bv�Bm�BdZB]/BZBT�BN�B@�B5?B'�B�B{BPB��B�fB�B��BȴBĜB�dB�!B��B��B�\B�JB�Br�BdZBXBA�B%�B�B  B
�B
�BB
ĜB
��B
�bB
s�B
p�B
ffB
\)B
G�B
2-B
�B
	7B	��B	�B	�B	�?B	��B	��B	��B	�PB	�+B	~�B	hsB	W
B	B�B	1'B	(�B	!�B	uB	
=B	B��B�B�B�B�`B��BȴBĜB�qB�LB�'B�B�B��B��B��B��B��B�uB�bB�\B�=B�+B�B}�B|�Bz�By�Bx�Bu�Bt�Bs�Br�Bp�Bn�Bl�BjBhsBgmBffBgmBdZBdZBaHB\)BXBVBT�BS�BQ�BO�BN�BN�BL�BK�BI�BH�BI�BC�B@�B>wB=qB:^B9XB5?B5?B2-B2-B1'B0!B0!B,B-B-B,B)�B)�B+B,B'�B'�B'�B&�B%�B#�B"�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B&�B&�B%�B'�B(�B,B33B49B33B2-B1'B2-B33B5?B7LB8RB8RB:^B;dB>wB@�BA�BA�BE�BF�BH�BK�BM�BP�BS�BT�BXBZBZB[#B\)BaHBiyBo�Bp�Bl�Bp�Bs�Br�Bq�Bs�Bx�Bz�B{�B~�B�B�B�1B�DB�VB�VB�VB�VB�hB�{B��B��B��B��B��B��B��B��B��B�B�!B�-B�9B�RB�^B�wBB��B��B��B�B�)B�TB�ZB�mB�B�B��B��B��B��B	  B	B	B	%B	+B		7B	
=B	PB	PB	\B	oB	�B	�B	�B	�B	�B	 �B	!�B	#�B	%�B	&�B	'�B	'�B	'�B	-B	2-B	49B	49B	5?B	7LB	:^B	;dB	<jB	<jB	=qB	=qB	>wB	A�B	B�B	D�B	F�B	I�B	M�B	Q�B	Q�B	VB	YB	\)B	_;B	`BB	aHB	cTB	cTB	dZB	hsB	k�B	l�B	l�B	l�B	m�B	m�B	m�B	m�B	m�B	n�B	p�B	t�B	u�B	u�B	w�B	x�B	x�B	y�B	|�B	|�B	}�B	}�B	� B	�B	�%B	�1B	�1B	�7B	�JB	�VB	�\B	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�3B	�9B	�LB	�RB	�^B	�jB	��B	��B	B	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�/B	�5B	�;B	�HB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
PB
PB
VB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
!�B
!�B
#�B
$�B
%�B
%�B
%�B
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
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
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
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
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
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
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
`BB
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
bNB
bNB
bNB
cTB
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
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
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
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
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
s�B
s�B
t�B
t�B
t�B
t�B
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
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
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
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B!�B!�B"�B#�B.B49B5?B5?B1'B,B$�B(�B0!B5?B;dBB�BbNBs�B�=B��B�'B�
B�B��B��BÖB�?B�B�-B��BB��B�5B��BBBB��B��B��B��BB�B2-BH�B7LB/B7LB]/BdZBl�Be`BjBs�Bu�Bp�B|�B{�By�Bx�Bz�B}�B{�Bv�Bm�BdZB]/BZBT�BN�B@�B5?B'�B�B{BPB��B�fB�B��BȴBĜB�dB�!B��B��B�\B�JB�Br�BdZBXBA�B%�B�B  B
�B
�BB
ĜB
��B
�bB
s�B
p�B
ffB
\)B
G�B
2-B
�B
	7B	��B	�B	�B	�?B	��B	��B	��B	�PB	�+B	~�B	hsB	W
B	B�B	1'B	(�B	!�B	uB	
=B	B��B�B�B�B�`B��BȴBĜB�qB�LB�'B�B�B��B��B��B��B��B�uB�bB�\B�=B�+B�B}�B|�Bz�By�Bx�Bu�Bt�Bs�Br�Bp�Bn�Bl�BjBhsBgmBffBgmBdZBdZBaHB\)BXBVBT�BS�BQ�BO�BN�BN�BL�BK�BI�BH�BI�BC�B@�B>wB=qB:^B9XB5?B5?B2-B2-B1'B0!B0!B,B-B-B,B)�B)�B+B,B'�B'�B'�B&�B%�B#�B"�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B&�B&�B%�B'�B(�B,B33B49B33B2-B1'B2-B33B5?B7LB8RB8RB:^B;dB>wB@�BA�BA�BE�BF�BH�BK�BM�BP�BS�BT�BXBZBZB[#B\)BaHBiyBo�Bp�Bl�Bp�Bs�Br�Bq�Bs�Bx�Bz�B{�B~�B�B�B�1B�DB�VB�VB�VB�VB�hB�{B��B��B��B��B��B��B��B��B��B�B�!B�-B�9B�RB�^B�wBB��B��B��B�B�)B�TB�ZB�mB�B�B��B��B��B��B	  B	B	B	%B	+B		7B	
=B	PB	PB	\B	oB	�B	�B	�B	�B	�B	 �B	!�B	#�B	%�B	&�B	'�B	'�B	'�B	-B	2-B	49B	49B	5?B	7LB	:^B	;dB	<jB	<jB	=qB	=qB	>wB	A�B	B�B	D�B	F�B	I�B	M�B	Q�B	Q�B	VB	YB	\)B	_;B	`BB	aHB	cTB	cTB	dZB	hsB	k�B	l�B	l�B	l�B	m�B	m�B	m�B	m�B	m�B	n�B	p�B	t�B	u�B	u�B	w�B	x�B	x�B	y�B	|�B	|�B	}�B	}�B	� B	�B	�%B	�1B	�1B	�7B	�JB	�VB	�\B	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�3B	�9B	�LB	�RB	�^B	�jB	��B	��B	B	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�/B	�5B	�;B	�HB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
PB
PB
VB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
!�B
!�B
#�B
$�B
%�B
%�B
%�B
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
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
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
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
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
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
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
`BB
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
bNB
bNB
bNB
cTB
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
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
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
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
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
s�B
s�B
t�B
t�B
t�B
t�B
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
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
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
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.36 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20200628110122                              AO  ARCAADJP                                                                    20200628110122    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200628110122  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200628110122  QCF$                G�O�G�O�G�O�0               
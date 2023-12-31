CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-04-19T09:00:41Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  `    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ol   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۔   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ޔ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20210419090041  20210419090041  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               gA   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @�nVS��1   @�nV����@;��$�/�c���l�D1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         gA   A   F   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @w
>@��@��A��A8��AX��Ax��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB&=qB.=qB6=qB>=qBF=qBN=qBV=qB^=qBf=qBn=qBv=qB~=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/u�C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC��{C�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮC�ǮD c�D ��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��D	c�D	��D
c�D
��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��Dc�D��D c�D ��D!c�D!��D"c�D"��D#c�D#��D$c�D$��D%c�D%��D&c�D&��D'c�D'��D(c�D(��D)c�D)��D*c�D*��D+c�D+��D,c�D,��D-c�D-��D.c�D.��D/c�D/��D0c�D0��D1c�D1��D2c�D2��D3c�D3��D4c�D4��D5c�D5��D6c�D6��D7c�D7��D8c�D8��D9c�D9��D:c�D:��D;c�D;��D<c�D<��D=c�D=��D>c�D>��D?c�D?��D@c�D@��DAc�DA��DBc�DB��DCc�DC��DDc�DD��DEc�DE��DFc�DF��DGc�DG��DHc�DH��DIc�DI��DJc�DJ��DKc�DK��DLc�DL��DMc�DM��DNj=DN��DOc�DO��DPc�DP��DQc�DQ��DRc�DR��DSc�DS��DTc�DT��DUc�DU��DVc�DV��DWc�DW��DXc�DX��DYc�DY��DZc�DZ��D[c�D[��D\c�D\��D]c�D]��D^c�D^��D_c�D_��D`c�D`��Dac�Da��Dbc�Db��Dcj=Dc��Ddc�Dd��Dec�De��Dfc�Df��Dgc�Dg��Dhc�Dh��Dic�Di��Djc�Dj��Dkc�Dk��Dlc�Dl��Dmc�Dm��Dnc�Dn�qDo]qDo��Dpc�Dp��Dqc�Dq��Drc�Dr��Dsc�Ds��Dtc�Dt��Duc�Du��Dvc�Dv��Dwc�Dw��Dxc�Dx��Dyc�Dy��Dzc�Dz��D{c�D{��D|c�D|��D}c�D}��D~c�D~��Dc�D��D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�.�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D±�D���D�1�D�q�Dñ�D���D�1�D�q�Dı�D���D�1�D�q�Dű�D���D�1�D�q�DƱ�D���D�1�D�q�DǱ�D���D�1�D�q�Dȱ�D���D�1�D�q�Dɱ�D���D�1�D�q�Dʱ�D���D�1�D�q�D˱�D���D�1�D�q�Ḏ�D���D�1�D�q�Dͱ�D���D�1�D�q�Dα�D���D�1�D�q�Dϱ�D���D�1�D�q�Dб�D���D�1�D�q�Dѱ�D���D�1�D�q�Dұ�D���D�1�D�q�Dӱ�D���D�1�D�q�DԱ�D���D�1�D�q�Dձ�D���D�1�D�q�Dֱ�D���D�1�D�q�Dױ�D���D�1�D�q�Dر�D���D�1�D�q�Dٱ�D���D�1�D�q�Dڱ�D���D�1�D�q�D۱�D���D�1�D�q�Dܱ�D���D�1�D�q�Dݱ�D���D�1�D�q�Dޱ�D���D�1�D�q�D߱�D���D�1�D�q�D��D���D�5D�q�D��D���D�1�D�q�D��D���D�1�D�q�D��D���D�1�D�q�D��D���D�1�D�q�D��D���D�1�D�q�D��D���D�1�D�q�D��D���D�1�D�q�D��D���D�1�D�q�D鮹D���D�1�D�q�D��D���D�1�D�q�D��D���D�1�D�q�D��D���D�1�D�q�D���D���D�1�D�q�D��D���D�1�D�q�D��D���D�1�D�q�D��D���D�1�D�q�D��D���D�1�D�uD��D���D�1�D�q�D��D���D�1�D�q�D���D���D�1�D�q�D���D���D�1�D�q�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��
A���A�ƨA�ȴA�ĜA���A���A�x�A�$�A��\A��DA�ffA�`BA�^5A�M�A�A�A�33A�+A�&�A��A��A�bA�A���A��A��A��A��A��mA��TA��A���A�ĜA��A��uA��A�hsA�?}A�
=A���A�JA�XA�;dA�{A�?}A�n�A�A�oA��A���A�bNA�bNA��A��PA�E�A���A�K�A��HA��RA��TA��+A���A��A�/A��A�1'A���A�A���A�hsA�I�A���A��PA���A�-A���A�ZA���A�M�A�dZA��A��PA�1'A��A���A��A���A�A�A��HA�A��A�z�A��9A�A`BA}�A{��Az�Ax��AvĜAu��At�+At-AtJAs��As��AqAn�RAm��Am;dAl�RAkoAh��Ag�-Af�Ad�+Ac|�Abn�A_A]oA\jA\{A[�^AZ�HAY�7AXbAV��AU��AS\)AQ�AQ?}AP�AP�+AP  AO�ANM�AM�ALVAK�AJM�AI`BAH��AH��AH�!AHM�AGhsAF��AF�AES�ADVAC;dAB{A@��A@$�A?�^A>�9A=�
A<�yA<{A;�A;�A:Q�A9�7A8z�A8bA6��A5A5|�A4�!A3��A3G�A2��A1x�A0^5A/��A/�PA/�A.��A.jA-�FA,n�A+�7A*ĜA(�HA'A'�A'oA&�9A&I�A%G�A$�jA$E�A$-A$JA#�TA#��A"��A!�A �`A I�A��AG�A��AK�A�HA=qAhsA��AVA�hAM�A7LA�yA��A�A�mA\)AbA�FAdZA��A�FA��A�AS�A
=A�A��A��A
(�A	�^A	x�A	l�A	?}A�jA�A��A��A`BAM�At�AC�A��AVA�hA~�AA
=A ��A r�@��@�7L@���@���@�E�@���@���@�z�@�=q@���@�;d@��#@�7L@�%@��@�@��@�x�@�bN@��@�7@��@�!@�@��@�j@ޟ�@��`@��;@ڇ+@�7L@�S�@ԃ@�
=@�O�@���@��@͑h@��@��@�7L@��@�j@�|�@��#@���@�?}@�v�@���@���@��h@�p�@�?}@���@�z�@�I�@�(�@��;@�K�@���@�7L@��/@�Q�@�1@���@�@��R@���@�n�@�J@��h@��@�r�@�o@���@�bN@���@�v�@�O�@�r�@�\)@��y@�~�@�@�@�p�@��@���@�$�@��#@�O�@�bN@�1@��@���@�\)@��@�5?@�?}@�Q�@�S�@�o@�ȴ@��!@�v�@�J@���@�&�@�%@��/@�(�@��w@�\)@�n�@��-@�?}@���@��@��@���@�v�@�E�@��@���@���@�O�@��@�9X@�  @��;@�ƨ@��w@��@���@���@��@��@�|�@�\)@��@�{@���@�x�@�O�@�%@�Z@�S�@���@�-@�@��@��-@��h@�hs@�%@�j@�1'@���@��F@���@�|�@�dZ@�;d@�
=@���@�V@�5?@�5?@��@��T@�@��-@�7L@��/@���@�j@�I�@�1'@��;@���@��@�K�@��@��H@��+@�n�@�5?@��T@���@���@���@�A�@�b@��P@�l�@�S�@�33@��@��R@��\@�ff@�-@���@�X@�/@�%@��@���@��u@�A�@�1@\)@+@
=@~�R@~v�@~{@}`B@}�@}V@|�@{��@{"�@z�!@z=q@y��@y�#@y��@y��@y&�@xr�@w�@w�@w|�@w\)@w;d@w
=@v��@v@up�@t�@tZ@s��@sS�@r�H@r��@r�@q��@pĜ@p�@p �@o�P@o
=@n�@n�+@nff@n@m��@m�-@m�@mO�@l��@l�/@lZ@kƨ@k��@k��@kt�@kC�@ko@j�@jn�@iX@i%@h�@h  @g;d@g+@f�@fff@f$�@f@e�@e�-@e�h@eO�@d�/@c�
@c��@ct�@cS�@c33@b�H@b��@b^5@b-@b-@b�@b�@bJ@a��@a�@a�^@ax�@`��@`bN@`b@_�@_��@_�w@_��@_�@^�R@^�+@^ff@^E�@^{@]�T@]�-@]�h@]O�@\��@\j@\j@[ƨ@[o@[@Z�@Z��@Z��@Z-@Yx�@Y�@X��@XQ�@Xb@Xb@W��@WK�@V�R@V�+@VV@U�h@T��@T�/@T��@T(�@St�@S"�@So@R�@R�H@R��@R�!@R=q@Q��@Q�7@QX@QG�@Q7L@Q&�@P��@P�9@PĜ@PQ�@O\)@N�y@M�T@M�@M`B@M?}@M�@L�j@Lz�@LI�@Kƨ@KS�@KC�@K33@J�@J�@I�@I��@I&�@I�@I%@H�`@H��@H�@HbN@H1'@H  @G��@G��@G\)@G+@G
=@F�y@F�R@F��@FV@E�@E��@E�h@EV@D�/@D��@D��@DI�@C��@C��@CdZ@CC�@C"�@C@B��@Bn�@B-@A�#@AG�@@�`@@�@@A�@@ �@@  @?�@?�;@?�;@?��@?�w@?�P@?l�@?+@>��@>�@>��@>v�@>5?@=@=O�@<��@<I�@;t�@;dZ@;C�@;"�@;@:�!@:-@9�@9��@9��@9X@9G�@9�@8�`@8��@8�@8  @7�;@7�;@7��@7�w@7�w@7�@7|�@7;d@7�@6��@6��@6ff@65?@5�@5�-@5�@5O�@5V@5V@5V@4�@4�j@4��@4(�@3C�@2�H@2�\@2~�@2n�@2M�@2M�@2=q@2�@1�#@1��@1��@1��@1��@1��@1x�@1G�@0Ĝ@0��@0��@0�@0bN@0bN@0bN@0A�@/�;@/�w@/�@/�@/\)@.ȴ@.��@.�+@.�+@.�+@.V@-�T@-�-@-�-@-p�@-`B@-�@,z�@,�@+�
@+ƨ@+��@+S�@*�H@*~�@*M�@*�@*J@)�@)��@)�^@)��@)x�@)X@)�@(Ĝ@(Ĝ@(Ĝ@(�9@(�u@(  @'K�@&�y@&ȴ@&��@&{@%��@%�@%`B@%?}@%�@$�j@$I�@#ƨ@#@"�!@"�\@"^5@"-@!��@!�#@!�^@!��@!x�@!X@!�@!�@!%@ Ĝ@ Q�@�;@��@|�@\)@+@�y@��@ff@E�@{@@@p�@p�@`B@?}@�@V@��@��@��@�@��@��@~�@n�@^5@=q@�@�@�#@��@��@�7@x�@7L@%@Ĝ@�9@�9@��@�u@ �@l�@�@
=@�R@�+@$�@�T@�-@�h@�h@�h@p�@?}@�@��@z�@I�@�@1@��@�m@�
@ƨ@�F@�F@��@�@"�@�@�H@��@�!@�\@^5@-@�@&�@��@��@�9@�u@r�@Q�@A�@b@�;@�w@�P@l�@K�@
=@��@�y@ȴ@��@�+@V@�@@��@�@p�@?}@�@�@�j@�@��@z�@9X@(�@(�@(�@1@�
@�F@�@dZ@"�@
��@
��@
�\@
n�@
n�@
M�@
-@
�@
�@
�@
J@	�#@	�^@	x�@	7L@��@�u@r�@Q�@1'@  @�;@|�@ȴ@��@��@v�@V@$�@@�T@��@�@`B@`B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��
A���A�ƨA�ȴA�ĜA���A���A�x�A�$�A��\A��DA�ffA�`BA�^5A�M�A�A�A�33A�+A�&�A��A��A�bA�A���A��A��A��A��A��mA��TA��A���A�ĜA��A��uA��A�hsA�?}A�
=A���A�JA�XA�;dA�{A�?}A�n�A�A�oA��A���A�bNA�bNA��A��PA�E�A���A�K�A��HA��RA��TA��+A���A��A�/A��A�1'A���A�A���A�hsA�I�A���A��PA���A�-A���A�ZA���A�M�A�dZA��A��PA�1'A��A���A��A���A�A�A��HA�A��A�z�A��9A�A`BA}�A{��Az�Ax��AvĜAu��At�+At-AtJAs��As��AqAn�RAm��Am;dAl�RAkoAh��Ag�-Af�Ad�+Ac|�Abn�A_A]oA\jA\{A[�^AZ�HAY�7AXbAV��AU��AS\)AQ�AQ?}AP�AP�+AP  AO�ANM�AM�ALVAK�AJM�AI`BAH��AH��AH�!AHM�AGhsAF��AF�AES�ADVAC;dAB{A@��A@$�A?�^A>�9A=�
A<�yA<{A;�A;�A:Q�A9�7A8z�A8bA6��A5A5|�A4�!A3��A3G�A2��A1x�A0^5A/��A/�PA/�A.��A.jA-�FA,n�A+�7A*ĜA(�HA'A'�A'oA&�9A&I�A%G�A$�jA$E�A$-A$JA#�TA#��A"��A!�A �`A I�A��AG�A��AK�A�HA=qAhsA��AVA�hAM�A7LA�yA��A�A�mA\)AbA�FAdZA��A�FA��A�AS�A
=A�A��A��A
(�A	�^A	x�A	l�A	?}A�jA�A��A��A`BAM�At�AC�A��AVA�hA~�AA
=A ��A r�@��@�7L@���@���@�E�@���@���@�z�@�=q@���@�;d@��#@�7L@�%@��@�@��@�x�@�bN@��@�7@��@�!@�@��@�j@ޟ�@��`@��;@ڇ+@�7L@�S�@ԃ@�
=@�O�@���@��@͑h@��@��@�7L@��@�j@�|�@��#@���@�?}@�v�@���@���@��h@�p�@�?}@���@�z�@�I�@�(�@��;@�K�@���@�7L@��/@�Q�@�1@���@�@��R@���@�n�@�J@��h@��@�r�@�o@���@�bN@���@�v�@�O�@�r�@�\)@��y@�~�@�@�@�p�@��@���@�$�@��#@�O�@�bN@�1@��@���@�\)@��@�5?@�?}@�Q�@�S�@�o@�ȴ@��!@�v�@�J@���@�&�@�%@��/@�(�@��w@�\)@�n�@��-@�?}@���@��@��@���@�v�@�E�@��@���@���@�O�@��@�9X@�  @��;@�ƨ@��w@��@���@���@��@��@�|�@�\)@��@�{@���@�x�@�O�@�%@�Z@�S�@���@�-@�@��@��-@��h@�hs@�%@�j@�1'@���@��F@���@�|�@�dZ@�;d@�
=@���@�V@�5?@�5?@��@��T@�@��-@�7L@��/@���@�j@�I�@�1'@��;@���@��@�K�@��@��H@��+@�n�@�5?@��T@���@���@���@�A�@�b@��P@�l�@�S�@�33@��@��R@��\@�ff@�-@���@�X@�/@�%@��@���@��u@�A�@�1@\)@+@
=@~�R@~v�@~{@}`B@}�@}V@|�@{��@{"�@z�!@z=q@y��@y�#@y��@y��@y&�@xr�@w�@w�@w|�@w\)@w;d@w
=@v��@v@up�@t�@tZ@s��@sS�@r�H@r��@r�@q��@pĜ@p�@p �@o�P@o
=@n�@n�+@nff@n@m��@m�-@m�@mO�@l��@l�/@lZ@kƨ@k��@k��@kt�@kC�@ko@j�@jn�@iX@i%@h�@h  @g;d@g+@f�@fff@f$�@f@e�@e�-@e�h@eO�@d�/@c�
@c��@ct�@cS�@c33@b�H@b��@b^5@b-@b-@b�@b�@bJ@a��@a�@a�^@ax�@`��@`bN@`b@_�@_��@_�w@_��@_�@^�R@^�+@^ff@^E�@^{@]�T@]�-@]�h@]O�@\��@\j@\j@[ƨ@[o@[@Z�@Z��@Z��@Z-@Yx�@Y�@X��@XQ�@Xb@Xb@W��@WK�@V�R@V�+@VV@U�h@T��@T�/@T��@T(�@St�@S"�@So@R�@R�H@R��@R�!@R=q@Q��@Q�7@QX@QG�@Q7L@Q&�@P��@P�9@PĜ@PQ�@O\)@N�y@M�T@M�@M`B@M?}@M�@L�j@Lz�@LI�@Kƨ@KS�@KC�@K33@J�@J�@I�@I��@I&�@I�@I%@H�`@H��@H�@HbN@H1'@H  @G��@G��@G\)@G+@G
=@F�y@F�R@F��@FV@E�@E��@E�h@EV@D�/@D��@D��@DI�@C��@C��@CdZ@CC�@C"�@C@B��@Bn�@B-@A�#@AG�@@�`@@�@@A�@@ �@@  @?�@?�;@?�;@?��@?�w@?�P@?l�@?+@>��@>�@>��@>v�@>5?@=@=O�@<��@<I�@;t�@;dZ@;C�@;"�@;@:�!@:-@9�@9��@9��@9X@9G�@9�@8�`@8��@8�@8  @7�;@7�;@7��@7�w@7�w@7�@7|�@7;d@7�@6��@6��@6ff@65?@5�@5�-@5�@5O�@5V@5V@5V@4�@4�j@4��@4(�@3C�@2�H@2�\@2~�@2n�@2M�@2M�@2=q@2�@1�#@1��@1��@1��@1��@1��@1x�@1G�@0Ĝ@0��@0��@0�@0bN@0bN@0bN@0A�@/�;@/�w@/�@/�@/\)@.ȴ@.��@.�+@.�+@.�+@.V@-�T@-�-@-�-@-p�@-`B@-�@,z�@,�@+�
@+ƨ@+��@+S�@*�H@*~�@*M�@*�@*J@)�@)��@)�^@)��@)x�@)X@)�@(Ĝ@(Ĝ@(Ĝ@(�9@(�u@(  @'K�@&�y@&ȴ@&��@&{@%��@%�@%`B@%?}@%�@$�j@$I�@#ƨ@#@"�!@"�\@"^5@"-@!��@!�#@!�^@!��@!x�@!X@!�@!�@!%@ Ĝ@ Q�@�;@��@|�@\)@+@�y@��@ff@E�@{@@@p�@p�@`B@?}@�@V@��@��@��@�@��@��@~�@n�@^5@=q@�@�@�#@��@��@�7@x�@7L@%@Ĝ@�9@�9@��@�u@ �@l�@�@
=@�R@�+@$�@�T@�-@�h@�h@�h@p�@?}@�@��@z�@I�@�@1@��@�m@�
@ƨ@�F@�F@��@�@"�@�@�H@��@�!@�\@^5@-@�@&�@��@��@�9@�u@r�@Q�@A�@b@�;@�w@�P@l�@K�@
=@��@�y@ȴ@��@�+@V@�@@��@�@p�@?}@�@�@�j@�@��@z�@9X@(�@(�@(�@1@�
@�F@�@dZ@"�@
��@
��@
�\@
n�@
n�@
M�@
-@
�@
�@
�@
J@	�#@	�^@	x�@	7L@��@�u@r�@Q�@1'@  @�;@|�@ȴ@��@��@v�@V@$�@@�T@��@�@`B@`B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B5?B5?B5?B6FB5?B5?B5?B5?B5?B6FB9XBQ�BVBR�BT�BVBT�BS�BS�BR�BR�BR�BR�BR�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BN�BK�BJ�BI�BF�BA�B=qB5?B/B/B+B�B1B  B�B��B�wB�FB��B�bB�7B�B�Bz�Bt�Bk�B]/BT�BK�B>wB1'B.B%�BoB+BBB��B��B�B�fB��B�3B�B��B��B�VB|�BgmBVBE�B<jB33B$�B�B�B1B��B��B�yB�B�)B��B�wB�B��B��B�oB�=B�%B�B�B�B}�Be`BaHB^5BYBR�BK�BC�B=qB1'B'�B!�B{BB
��B
��B
��B
��B
�B
�sB
�BB
�#B
��B
ŢB
ÖB
��B
�}B
�jB
�RB
�9B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�hB
�PB
�DB
�%B
�B
}�B
y�B
x�B
t�B
q�B
n�B
jB
iyB
hsB
hsB
gmB
e`B
cTB
]/B
ZB
XB
W
B
S�B
S�B
T�B
R�B
N�B
M�B
K�B
J�B
I�B
F�B
E�B
A�B
=qB
9XB
5?B
.B
,B
)�B
'�B
$�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
bB
PB
DB

=B
+B
B
B
B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�sB	�fB	�ZB	�TB	�NB	�BB	�5B	�)B	�)B	�#B	�B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ǮB	ǮB	ƨB	ĜB	ĜB	ÖB	B	��B	��B	�}B	�wB	�qB	�qB	�jB	�dB	�dB	�XB	�XB	�XB	�RB	�RB	�LB	�LB	�LB	�?B	�?B	�?B	�3B	�-B	�-B	�!B	�!B	�!B	�!B	�B	�B	�B	�B	�!B	�!B	�B	�'B	�'B	�!B	�-B	�9B	�9B	�?B	�?B	�?B	�FB	�RB	�^B	�wB	��B	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ŢB	ĜB	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�;B	�ZB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
%B
	7B
VB
bB
bB
hB
hB
uB
{B
�B
�B
�B
�B
�B
 �B
$�B
(�B
)�B
2-B
5?B
8RB
;dB
;dB
<jB
=qB
?}B
?}B
A�B
E�B
F�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
N�B
R�B
VB
XB
YB
[#B
^5B
cTB
ffB
k�B
n�B
o�B
r�B
s�B
t�B
x�B
}�B
� B
�B
�%B
�+B
�1B
�7B
�7B
�=B
�JB
�PB
�JB
�JB
�PB
�VB
�VB
�PB
�VB
�VB
�bB
�hB
�oB
�uB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�'B
�9B
�?B
�^B
�dB
�jB
�qB
��B
B
ƨB
ɺB
��B
��B
��B
��B
��B
��B
��B
�
B
�B
�)B
�/B
�/B
�5B
�;B
�BB
�HB
�TB
�`B
�mB
�yB
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B  BBBB1BDBPBbBoBuB{B�B�B�B�B�B�B!�B"�B#�B#�B%�B&�B'�B'�B)�B,B-B/B1'B2-B2-B33B49B5?B6FB7LB;dB<jB>wB@�BC�BD�BE�BG�BH�BI�BI�BK�BK�BL�BN�BS�BS�BT�BVBVBW
BXBYBZBZBZB[#B[#B[#B\)B\)B]/B_;BaHBbNBbNBcTBcTBdZBffBhsBiyBiyBjBk�Bk�Bl�Bl�Bn�Bo�Bp�Bp�Bs�Bt�Bu�Bu�Bu�Bv�Bx�B{�B{�B}�B}�B~�B~�B~�B� B�B�B�B�B�+B�+B�1B�7B�DB�JB�PB�PB�VB�VB�VB�bB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�'B�-B�-B�-B�-B�3B�9B�9B�?B�?B�?B�FB�FB�LB�LB�RB�XB�XB�^B�^B�dB�dB�dB�dB�dB�jB�jB�jB�jB�qB�qB�qB�wB�wB�}B��B��BBBĜBĜBĜBĜBĜBŢBƨBƨBǮBǮBǮBǮBȴBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�
B�
B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�#B�#B�)B�)B�)B�)B�)B�/B�/B�5B�5B�5B�;B�;B�BB�BB�HB�HB�HB�HB�HB�HB�HB�NB�NB�NB�NB�NB�NB�NB�TB�`B�`B�`B�`B�fB�mB�mB�mB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  B  B  B  BBBBBBBBBBBBBBBBBBBBBBBBB%B%B+B+B+B+B1B1B1B1B1B1B	7B	7B	7B	7B	7B
=B
=B
=B
=BDBDBDBDBJBJBJBJBPBPBPBPBPBPBPBPBVBVBVBVBVB\B\B\BbBbBbBbBbBbBbBbBbBhBhBhBhBoBoBoBuBuBuBuB{B�B�B�B�B�B�B�B�B�B�B�B�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B5?B5?B5?B6FB5?B5?B5?B5?B5?B6FB9XBQ�BVBR�BT�BVBT�BS�BS�BR�BR�BR�BR�BR�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BN�BK�BJ�BI�BF�BA�B=qB5?B/B/B+B�B1B  B�B��B�wB�FB��B�bB�7B�B�Bz�Bt�Bk�B]/BT�BK�B>wB1'B.B%�BoB+BBB��B��B�B�fB��B�3B�B��B��B�VB|�BgmBVBE�B<jB33B$�B�B�B1B��B��B�yB�B�)B��B�wB�B��B��B�oB�=B�%B�B�B�B}�Be`BaHB^5BYBR�BK�BC�B=qB1'B'�B!�B{BB
��B
��B
��B
��B
�B
�sB
�BB
�#B
��B
ŢB
ÖB
��B
�}B
�jB
�RB
�9B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�hB
�PB
�DB
�%B
�B
}�B
y�B
x�B
t�B
q�B
n�B
jB
iyB
hsB
hsB
gmB
e`B
cTB
]/B
ZB
XB
W
B
S�B
S�B
T�B
R�B
N�B
M�B
K�B
J�B
I�B
F�B
E�B
A�B
=qB
9XB
5?B
.B
,B
)�B
'�B
$�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
bB
PB
DB

=B
+B
B
B
B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�sB	�fB	�ZB	�TB	�NB	�BB	�5B	�)B	�)B	�#B	�B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ǮB	ǮB	ƨB	ĜB	ĜB	ÖB	B	��B	��B	�}B	�wB	�qB	�qB	�jB	�dB	�dB	�XB	�XB	�XB	�RB	�RB	�LB	�LB	�LB	�?B	�?B	�?B	�3B	�-B	�-B	�!B	�!B	�!B	�!B	�B	�B	�B	�B	�!B	�!B	�B	�'B	�'B	�!B	�-B	�9B	�9B	�?B	�?B	�?B	�FB	�RB	�^B	�wB	��B	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ŢB	ĜB	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�;B	�ZB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
%B
	7B
VB
bB
bB
hB
hB
uB
{B
�B
�B
�B
�B
�B
 �B
$�B
(�B
)�B
2-B
5?B
8RB
;dB
;dB
<jB
=qB
?}B
?}B
A�B
E�B
F�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
N�B
R�B
VB
XB
YB
[#B
^5B
cTB
ffB
k�B
n�B
o�B
r�B
s�B
t�B
x�B
}�B
� B
�B
�%B
�+B
�1B
�7B
�7B
�=B
�JB
�PB
�JB
�JB
�PB
�VB
�VB
�PB
�VB
�VB
�bB
�hB
�oB
�uB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�'B
�9B
�?B
�^B
�dB
�jB
�qB
��B
B
ƨB
ɺB
��B
��B
��B
��B
��B
��B
��B
�
B
�B
�)B
�/B
�/B
�5B
�;B
�BB
�HB
�TB
�`B
�mB
�yB
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B  BBBB1BDBPBbBoBuB{B�B�B�B�B�B�B!�B"�B#�B#�B%�B&�B'�B'�B)�B,B-B/B1'B2-B2-B33B49B5?B6FB7LB;dB<jB>wB@�BC�BD�BE�BG�BH�BI�BI�BK�BK�BL�BN�BS�BS�BT�BVBVBW
BXBYBZBZBZB[#B[#B[#B\)B\)B]/B_;BaHBbNBbNBcTBcTBdZBffBhsBiyBiyBjBk�Bk�Bl�Bl�Bn�Bo�Bp�Bp�Bs�Bt�Bu�Bu�Bu�Bv�Bx�B{�B{�B}�B}�B~�B~�B~�B� B�B�B�B�B�+B�+B�1B�7B�DB�JB�PB�PB�VB�VB�VB�bB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�'B�-B�-B�-B�-B�3B�9B�9B�?B�?B�?B�FB�FB�LB�LB�RB�XB�XB�^B�^B�dB�dB�dB�dB�dB�jB�jB�jB�jB�qB�qB�qB�wB�wB�}B��B��BBBĜBĜBĜBĜBĜBŢBƨBƨBǮBǮBǮBǮBȴBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�
B�
B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�#B�#B�)B�)B�)B�)B�)B�/B�/B�5B�5B�5B�;B�;B�BB�BB�HB�HB�HB�HB�HB�HB�HB�NB�NB�NB�NB�NB�NB�NB�TB�`B�`B�`B�`B�fB�mB�mB�mB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  B  B  B  BBBBBBBBBBBBBBBBBBBBBBBBB%B%B+B+B+B+B1B1B1B1B1B1B	7B	7B	7B	7B	7B
=B
=B
=B
=BDBDBDBDBJBJBJBJBPBPBPBPBPBPBPBPBVBVBVBVBVB\B\B\BbBbBbBbBbBbBbBbBbBhBhBhBhBoBoBoBuBuBuBuB{B�B�B�B�B�B�B�B�B�B�B�B�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.44 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210419090041                              AO  ARCAADJP                                                                    20210419090041    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210419090041  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210419090041  QCF$                G�O�G�O�G�O�8000            
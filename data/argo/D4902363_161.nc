CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-21T21:35:07Z creation;2017-09-21T21:35:11Z conversion to V3.1;2019-12-19T08:01:06Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170921213507  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_161                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�'��j�1   @�'��-�@:�8}�H�d��o h�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCu�fCw�fCz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ D�|�D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C\C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Cb\Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs�)Cu�)Cw�)Cy��C{��C}��C��C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�{�Dվ�D���D�>�D�~�D־�D���D�>�Dׁ�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D��D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A��A��A��A��A��A��A��A� �A� �A�"�A�$�A�"�A�&�A�+A�-A�5?A�A�A�K�A���A���A�I�A��A���A̩�A�z�A��#A�  A��TA��A�t�A���A�hsA�1'A��A���A�bA�I�A���A�ffA�9XA���A�M�A�jA�-A���A��`A�ȴA��A��A���A��#A�1A�{A�~�A��A�
=A���A�&�A�M�A�z�A�M�A��A�hsA�M�A��;A���A��A� �A��jA�%A��mA�1'A�C�A�;dA�oA��A��\A�&�A���A�ƨA�^5A��A��A�E�A��^A��A�`BA�VA��+A�33A���A��A}��A|�A|�A{�hAz��Az�Ax��AwhsAvM�Au�As��As?}Ar=qAq�Ap��Ao�FAnĜAnbAmG�Al��Al��Al �Ak��AkG�Aj�yAj��AjA�Aj1AiXAh^5AgƨAf�Ae�-Ad��Ac�AdbAc�AbE�Aa�wA`��A`z�A_7LA^r�A]A]�A\��AZ��AZJAYp�AX��AW��AW�AV�AT$�ASO�AR�AR��AR1AQ�AP�jAP~�APjAP�AO7LANI�AL�!AK�AJ �AI��AIO�AH��AH9XAG�AGK�AGVAF��AF(�AE��AD�AC�hAC
=AA��A@��A@VA@{A?�FA>ZA<z�A;"�A:~�A:E�A:�A9�A9��A9��A8E�A6�DA5�A4ĜA4��A4v�A4bNA4A�A41A3�#A2�+A1�A1VA0ȴA/O�A.=qA-�;A-�hA-S�A,�A,ffA+�A)��A'��A'O�A&�A&1A$�A${A#�
A#ƨA#��A#S�A#oA"�!A"1'A!x�A �uA�9A�A�PAC�A/A�A�^AZA�A�AȴA�A�A�AA�AG�A�yAE�A��Al�A��A��AK�AVA��A
�RA	��A	�A��A�A�#A�uAQ�A��A�7Al�AG�A�+A ��A A�@�/@��+@�(�@�v�@���@�33@�^5@�@�@��`@@�V@�@�?}@���@�D@�Z@띲@�J@��/@�l�@�o@�{@�9@���@���@ޏ\@�z�@۝�@�@�ƨ@��@�n�@��#@�&�@�1@�33@ҏ\@�E�@�?}@��@�9X@�ȴ@���@ʏ\@�n�@���@ɉ7@�Z@�  @��m@ǶF@ř�@ļj@�ƨ@�5?@���@�9X@��F@�ȴ@�{@��#@���@�`B@�j@���@�+@��@��@��@���@��7@�7L@��D@��@���@���@��y@���@��7@���@���@���@��+@�E�@��#@�/@�j@�ƨ@���@�O�@��D@�1'@�  @��w@��P@�K�@���@��@�Ĝ@�b@���@���@�dZ@�"�@��\@���@��T@���@�bN@���@��F@���@���@�j@���@��@�{@�r�@�b@���@���@�+@��@��y@���@���@��R@��+@�v�@�ff@�V@�=q@��@��#@�hs@�Z@�t�@��@��R@���@�E�@���@�G�@�z�@� �@�1@��w@��P@�;d@���@�~�@���@�O�@���@���@�bN@�b@�ƨ@���@�l�@�K�@�"�@�ȴ@��\@�n�@�@��#@���@�G�@�&�@��j@�9X@�  @��m@��w@�|�@�\)@�K�@���@��\@�5?@���@�@�G�@���@�A�@��m@�33@���@��H@���@���@��@��h@�p�@�V@�Ĝ@���@�bN@��@�w@~��@~ff@}�T@|�/@{�m@{@z��@y��@xr�@w�@w�;@w�;@w��@w+@v�R@vv�@v5?@u�T@u��@u�@t��@t(�@s��@s�
@sƨ@s��@sC�@r=q@q��@q&�@p��@pQ�@p1'@o�@o��@o�@o
=@m?}@l�/@l��@l�j@l�j@lI�@kt�@k"�@k"�@j�@j��@j�!@j�!@j��@j�\@jn�@jJ@i��@i��@i&�@i%@h�u@hA�@hb@h  @g�;@g�w@f��@fE�@e@ep�@d��@d��@d(�@c�F@c@bn�@b-@a�@ax�@`��@`�9@`bN@` �@`  @_�@_��@_�P@^�+@^V@]�T@]��@]O�@\�j@\�D@\j@\(�@[�m@[33@Z^5@ZJ@Y��@Y��@YG�@X�u@XA�@Xb@W�;@W�P@WK�@V�R@V{@U�-@U`B@T�@Tj@T(�@S�
@SdZ@SS�@S33@S"�@So@R�@R�\@R-@Q�@Q��@Q�7@P�9@O��@O�@Nv�@N$�@M�-@MO�@MV@L�@L�D@LI�@L(�@L1@K�
@Kt�@KC�@K33@K"�@Ko@Ko@K@J�H@J��@J��@J�\@JM�@J=q@JJ@I��@I7L@I%@HĜ@H��@HbN@G�@G;d@G�@F�@F�R@F��@Fv�@F$�@E�-@Ep�@E?}@D�j@D��@Dz�@DZ@D(�@C�
@CC�@B��@BM�@BJ@A�@A��@A��@Ax�@@�`@@1'@?�@?��@?�P@?\)@?
=@>��@>V@=�@=/@<�@<�j@<�j@<��@<�j@<�@<j@<9X@<(�@<�@;ƨ@;33@:M�@9�#@9��@9x�@97L@8�u@8  @7�@7
=@6��@6E�@6$�@6{@6@5�@5��@5��@4��@4I�@4(�@41@3��@3@2��@2��@2�!@2~�@2^5@2-@1��@0�9@/�@/�P@/|�@/�@.�R@.��@.�+@.v�@.ff@.5?@.@-��@-@-�-@-��@-`B@,��@,j@,�@+��@+ƨ@+ƨ@+��@+t�@+C�@+o@*�!@*�\@*�@)��@)��@)��@)x�@)�@(��@(�u@(r�@(1'@'�@'|�@'+@&��@&��@&E�@&E�@&E�@&{@%�-@%?}@$�@$��@$j@#��@#��@#��@#�F@#�@#t�@#t�@#@"��@"�!@"�\@"n�@"M�@"=q@"=q@!��@!&�@!%@!%@ �`@ ��@ ��@ �@�w@K�@�y@ȴ@ȴ@�R@��@E�@{@�@/@�/@��@z�@z�@j@(�@�m@�m@��@33@o@o@�H@�!@��@�\@��@�\@^5@�@X@�@��@�`@�`@Ĝ@��@��@�u@�u@�@r�@1'@�;@�@|�@\)@��@�R@��@v�@E�@@�-@p�@p�@?}@V@�@�j@�D@�D@��@Z@�@1@�m@�
@�F@t�@"�@�H@��@^5@��@�#@��@7L@�@�`@�`@�9@��@Q�@b@��@|�@l�@\)@K�@K�@K�@K�@
=@��@V@E�@$�@@��@��@�@�@O�@V@�@�/@��@�@z�@9X@��@�
@�F@dZ@C�@"�@o@
�@
�@
��@
��@
�!@
��@
�\@
�\@
~�@
~�@
~�@
~�@
n�@
J@	�7@	7L@	�@	�@	�@��@Ĝ@�u@�u@r�@bN@bN@Q�@A�@A�@1'@b@  @  @��@�@|�@\)@K�@+@+@�@�R@��@��@�+@v�@ff@E�@5?@@�-@�h@`B@O�@O�@O�@O�@?}@?}@/@�@�@�j@�j@�@��@��@�D@�D@z�@j@I�@I�@(�@(�@(�@�@1@�@1@1@1@1@��@�F@S�@"�@o@@��@=q@��@��@��@x�@G�@�@ ��@ �`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A��A��A��A��A��A��A��A� �A� �A�"�A�$�A�"�A�&�A�+A�-A�5?A�A�A�K�A���A���A�I�A��A���A̩�A�z�A��#A�  A��TA��A�t�A���A�hsA�1'A��A���A�bA�I�A���A�ffA�9XA���A�M�A�jA�-A���A��`A�ȴA��A��A���A��#A�1A�{A�~�A��A�
=A���A�&�A�M�A�z�A�M�A��A�hsA�M�A��;A���A��A� �A��jA�%A��mA�1'A�C�A�;dA�oA��A��\A�&�A���A�ƨA�^5A��A��A�E�A��^A��A�`BA�VA��+A�33A���A��A}��A|�A|�A{�hAz��Az�Ax��AwhsAvM�Au�As��As?}Ar=qAq�Ap��Ao�FAnĜAnbAmG�Al��Al��Al �Ak��AkG�Aj�yAj��AjA�Aj1AiXAh^5AgƨAf�Ae�-Ad��Ac�AdbAc�AbE�Aa�wA`��A`z�A_7LA^r�A]A]�A\��AZ��AZJAYp�AX��AW��AW�AV�AT$�ASO�AR�AR��AR1AQ�AP�jAP~�APjAP�AO7LANI�AL�!AK�AJ �AI��AIO�AH��AH9XAG�AGK�AGVAF��AF(�AE��AD�AC�hAC
=AA��A@��A@VA@{A?�FA>ZA<z�A;"�A:~�A:E�A:�A9�A9��A9��A8E�A6�DA5�A4ĜA4��A4v�A4bNA4A�A41A3�#A2�+A1�A1VA0ȴA/O�A.=qA-�;A-�hA-S�A,�A,ffA+�A)��A'��A'O�A&�A&1A$�A${A#�
A#ƨA#��A#S�A#oA"�!A"1'A!x�A �uA�9A�A�PAC�A/A�A�^AZA�A�AȴA�A�A�AA�AG�A�yAE�A��Al�A��A��AK�AVA��A
�RA	��A	�A��A�A�#A�uAQ�A��A�7Al�AG�A�+A ��A A�@�/@��+@�(�@�v�@���@�33@�^5@�@�@��`@@�V@�@�?}@���@�D@�Z@띲@�J@��/@�l�@�o@�{@�9@���@���@ޏ\@�z�@۝�@�@�ƨ@��@�n�@��#@�&�@�1@�33@ҏ\@�E�@�?}@��@�9X@�ȴ@���@ʏ\@�n�@���@ɉ7@�Z@�  @��m@ǶF@ř�@ļj@�ƨ@�5?@���@�9X@��F@�ȴ@�{@��#@���@�`B@�j@���@�+@��@��@��@���@��7@�7L@��D@��@���@���@��y@���@��7@���@���@���@��+@�E�@��#@�/@�j@�ƨ@���@�O�@��D@�1'@�  @��w@��P@�K�@���@��@�Ĝ@�b@���@���@�dZ@�"�@��\@���@��T@���@�bN@���@��F@���@���@�j@���@��@�{@�r�@�b@���@���@�+@��@��y@���@���@��R@��+@�v�@�ff@�V@�=q@��@��#@�hs@�Z@�t�@��@��R@���@�E�@���@�G�@�z�@� �@�1@��w@��P@�;d@���@�~�@���@�O�@���@���@�bN@�b@�ƨ@���@�l�@�K�@�"�@�ȴ@��\@�n�@�@��#@���@�G�@�&�@��j@�9X@�  @��m@��w@�|�@�\)@�K�@���@��\@�5?@���@�@�G�@���@�A�@��m@�33@���@��H@���@���@��@��h@�p�@�V@�Ĝ@���@�bN@��@�w@~��@~ff@}�T@|�/@{�m@{@z��@y��@xr�@w�@w�;@w�;@w��@w+@v�R@vv�@v5?@u�T@u��@u�@t��@t(�@s��@s�
@sƨ@s��@sC�@r=q@q��@q&�@p��@pQ�@p1'@o�@o��@o�@o
=@m?}@l�/@l��@l�j@l�j@lI�@kt�@k"�@k"�@j�@j��@j�!@j�!@j��@j�\@jn�@jJ@i��@i��@i&�@i%@h�u@hA�@hb@h  @g�;@g�w@f��@fE�@e@ep�@d��@d��@d(�@c�F@c@bn�@b-@a�@ax�@`��@`�9@`bN@` �@`  @_�@_��@_�P@^�+@^V@]�T@]��@]O�@\�j@\�D@\j@\(�@[�m@[33@Z^5@ZJ@Y��@Y��@YG�@X�u@XA�@Xb@W�;@W�P@WK�@V�R@V{@U�-@U`B@T�@Tj@T(�@S�
@SdZ@SS�@S33@S"�@So@R�@R�\@R-@Q�@Q��@Q�7@P�9@O��@O�@Nv�@N$�@M�-@MO�@MV@L�@L�D@LI�@L(�@L1@K�
@Kt�@KC�@K33@K"�@Ko@Ko@K@J�H@J��@J��@J�\@JM�@J=q@JJ@I��@I7L@I%@HĜ@H��@HbN@G�@G;d@G�@F�@F�R@F��@Fv�@F$�@E�-@Ep�@E?}@D�j@D��@Dz�@DZ@D(�@C�
@CC�@B��@BM�@BJ@A�@A��@A��@Ax�@@�`@@1'@?�@?��@?�P@?\)@?
=@>��@>V@=�@=/@<�@<�j@<�j@<��@<�j@<�@<j@<9X@<(�@<�@;ƨ@;33@:M�@9�#@9��@9x�@97L@8�u@8  @7�@7
=@6��@6E�@6$�@6{@6@5�@5��@5��@4��@4I�@4(�@41@3��@3@2��@2��@2�!@2~�@2^5@2-@1��@0�9@/�@/�P@/|�@/�@.�R@.��@.�+@.v�@.ff@.5?@.@-��@-@-�-@-��@-`B@,��@,j@,�@+��@+ƨ@+ƨ@+��@+t�@+C�@+o@*�!@*�\@*�@)��@)��@)��@)x�@)�@(��@(�u@(r�@(1'@'�@'|�@'+@&��@&��@&E�@&E�@&E�@&{@%�-@%?}@$�@$��@$j@#��@#��@#��@#�F@#�@#t�@#t�@#@"��@"�!@"�\@"n�@"M�@"=q@"=q@!��@!&�@!%@!%@ �`@ ��@ ��@ �@�w@K�@�y@ȴ@ȴ@�R@��@E�@{@�@/@�/@��@z�@z�@j@(�@�m@�m@��@33@o@o@�H@�!@��@�\@��@�\@^5@�@X@�@��@�`@�`@Ĝ@��@��@�u@�u@�@r�@1'@�;@�@|�@\)@��@�R@��@v�@E�@@�-@p�@p�@?}@V@�@�j@�D@�D@��@Z@�@1@�m@�
@�F@t�@"�@�H@��@^5@��@�#@��@7L@�@�`@�`@�9@��@Q�@b@��@|�@l�@\)@K�@K�@K�@K�@
=@��@V@E�@$�@@��@��@�@�@O�@V@�@�/@��@�@z�@9X@��@�
@�F@dZ@C�@"�@o@
�@
�@
��@
��@
�!@
��@
�\@
�\@
~�@
~�@
~�@
~�@
n�@
J@	�7@	7L@	�@	�@	�@��@Ĝ@�u@�u@r�@bN@bN@Q�@A�@A�@1'@b@  @  @��@�@|�@\)@K�@+@+@�@�R@��@��@�+@v�@ff@E�@5?@@�-@�h@`B@O�@O�@O�@O�@?}@?}@/@�@�@�j@�j@�@��@��@�D@�D@z�@j@I�@I�@(�@(�@(�@�@1@�@1@1@1@1@��@�F@S�@"�@o@@��@=q@��@��@��@x�@G�@�@ ��@ �`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BdZBcTBdZBdZBdZBdZBdZBe`Be`Be`BffBffBgmBgmBgmBgmBiyBjBk�Bp�Bw�By�BiyB��B�;B�fB�-B�LB�bBu�B��B��B�B�VBVBR�BN�BG�B7LB33B49B.B&�BBǮB�mBB  B��B��B�B�B�'B�?B�!B�!B�!B��B�B��B��B�BcTB{�Bx�Br�BjBiyBdZBYBP�BJ�B@�B.B�BDB
��B
��B
��B
��B
�B
�B
�BB
�B
��B
ƨB
�qB
�XB
�B
��B
��B
��B
��B
�bB
�%B
w�B
v�B
o�B
m�B
hsB
bNB
YB
N�B
C�B
>wB
<jB
>wB
8RB
1'B
-B
%�B
!�B
�B
�B
�B
�B
�B
�B
{B
uB
hB
\B
PB
1B
B	��B	�B	�HB	�B	��B	�B	�B	��B	��B	ÖB	ÖB	�wB	�qB	�RB	�?B	�'B	��B	��B	��B	��B	�{B	�oB	�DB	~�B	w�B	x�B	u�B	r�B	o�B	l�B	l�B	k�B	jB	dZB	\)B	S�B	J�B	F�B	F�B	C�B	A�B	=qB	9XB	9XB	8RB	5?B	5?B	5?B	/B	/B	+B	%�B	�B	�B	�B	�B	�B	\B	\B	bB	hB	bB	\B	VB	
=B	B��B��B��B��B��B��B��B�B�B�B�`B�TB�HB�#B�
B�
B��B��B��B��BĜB�jB�3B�FB�9B�B��B��B��B��B��B��B��B��B��B��B��B�VB�7B�B�+B�+B�B~�Bz�B|�B{�Bx�Bx�Bv�Br�Bo�Bk�BdZBgmBgmBdZBbNB_;B^5B[#BYBVBVBT�BR�BP�BN�BH�BC�BF�BG�BF�BE�BA�B:^B<jB6FB2-B1'B2-B0!B0!B33B33B49B2-B0!B1'B33B5?B6FB6FB7LB49B1'B33B1'B5?B2-B0!B,B.B,B/B2-B0!B1'B5?B8RB7LB6FB6FB7LB8RB9XB6FB2-B49B;dBB�BA�BA�B@�B@�B>wBB�BB�B@�B;dB@�B@�B@�BC�BH�BI�BH�BK�BM�BN�BM�BL�BN�BO�BQ�BR�BP�BR�BS�BR�BQ�BQ�BN�BR�B\)B^5B\)B\)BaHBgmBgmBgmBgmBffBgmBhsBiyBm�Br�Bu�Bw�Bw�Bx�Bx�Bw�Bw�B�B� B�B�%B�B�B�B�%B�7B�1B�%B�JB�PB�DB�VB��B��B��B��B��B�B�!B�'B�-B�3B�?B�FB�FB�LB�FB�LB�RB�RB�RB�RB�LB�LB�RB�wBĜBƨBǮBȴBɺB��B��B��B�
B�
B�B�B�#B�)B�)B�;B�ZB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	B	%B	+B	+B		7B		7B	1B	
=B	PB	bB	hB	hB	{B	�B	�B	�B	�B	!�B	"�B	"�B	#�B	&�B	-B	-B	1'B	33B	33B	49B	5?B	5?B	7LB	9XB	:^B	>wB	A�B	D�B	D�B	G�B	N�B	Q�B	Q�B	Q�B	P�B	Q�B	R�B	R�B	R�B	S�B	VB	W
B	XB	\)B	^5B	^5B	]/B	]/B	\)B	`BB	`BB	bNB	dZB	dZB	e`B	ffB	e`B	dZB	dZB	l�B	o�B	o�B	o�B	n�B	o�B	s�B	u�B	v�B	x�B	x�B	y�B	y�B	x�B	x�B	y�B	z�B	z�B	{�B	|�B	|�B	~�B	�B	�B	�B	�B	�B	�+B	�=B	�PB	�VB	�\B	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�9B	�9B	�?B	�FB	�RB	�^B	�dB	�jB	�qB	��B	��B	��B	B	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�#B	�)B	�)B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�ZB	�`B	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
	7B
	7B
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
\B
VB
\B
oB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
+B
+B
,B
,B
-B
.B
.B
-B
-B
.B
/B
0!B
0!B
1'B
2-B
49B
5?B
6FB
7LB
7LB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
7LB
7LB
9XB
9XB
9XB
9XB
8RB
8RB
7LB
7LB
8RB
9XB
:^B
:^B
9XB
9XB
9XB
9XB
:^B
;dB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
A�B
A�B
B�B
A�B
B�B
B�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
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
N�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
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
S�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
VB
VB
W
B
VB
W
B
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
^5B
_;B
aHB
aHB
aHB
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
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
ffB
ffB
ffB
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
iyB
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
jB
jB
jB
k�B
k�B
k�B
jB
k�B
k�B
l�B
l�B
m�B
l�B
m�B
n�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BdZBcTBdZBdZBdZBdZBdZBe`Be`Be`BffBffBgmBgmBgmBgmBiyBjBk�Bp�Bx�B}"Bs�B�1B�ZB�B��B�qB��B}�B�pB�BB��B�[B^�BW�BS�BLJB;�B6�B6�B/�B(XB�B��B�BUB iB�HB�rB�/BٴB��B�rB�nB�TB�MB�B�/B�$B��B�BgB|�By�BtnBl�BjBeFB[#BR:BLBB�B1B1BjB
��B
�]B
�jB
��B
��B
��B
�NB
�7B
�FB
ȴB
�cB
��B
��B
�8B
��B
��B
�mB
��B
��B
z*B
w�B
p�B
nIB
i_B
c�B
Z�B
P}B
EB
?�B
=�B
?cB
9�B
2GB
./B
'B
#B
 �B
�B
IB
�B
EB
B
�B
�B
�B
�B
�B
	B
'B	��B	��B	�B	�B	��B	�KB	�B	�@B	˒B	ĜB	āB	��B	�wB	�>B	�B	��B	��B	��B	��B	��B	��B	�[B	��B	�B	x�B	yXB	vFB	s�B	pUB	m]B	l�B	k�B	k6B	e�B	]�B	VB	L�B	G�B	GEB	DMB	BAB	>BB	:*B	9�B	8�B	5�B	6B	6FB	0�B	/�B	,B	'mB	�B	OB	OB	�B	sB	�B	 B	B	�B	�B	�B	�B	)B	B��B�B��B�+B��B�B�B�3B�[B�)B�B�B�4B��B�_B׍BՁB�uBϑB��B�%B��B�tB��B�B��B��B��B�KB�*B�XB�fB�`B��B��B��B�+B��B�DB��B��B�zB��B��B|�B}�B|�By�ByrBw�BtBp�BmCBgBh>Bh
BeFBcTB`vB_VB\�BZ7BW?BW$BU�BS�BQ�BP.BJ�BF%BGzBG�BGBF?BB�B<�B=�B8�B4B2�B3hB1vB1'B3�B3�B4�B2�B1B2B3�B5�B6�B6�B7�B4�B2aB4B2-B5�B3B1AB-�B/�B-�B0oB2�B1vB2aB5�B8�B7�B6�B7B7�B8�B9�B7LB3�B6B<6BB�BA�BA�BABAB?HBB�BB�BAB<�BA;BAoBA�BD�BIRBJXBIlBLJBN"BO(BNVBM�BO\BPbBR:BS&BQ�BS@BTFBSuBR�BR�BP}BTaB\�B^�B]/B]IBa�Bg�Bg�Bg�Bg�BgBh$BiBjBn}Bs3BvBxBxBy>By>Bx�Bx�B�UB��B��B�YB��B�mB��B��B�lB��B�B��B��B�dB��B�B�]B�;B��B��B�cB�UB�vB�|B�hB�ZB�zB�zB�fB�zB��B�lB�lB��B��B��B��B�$B�B��B��B��B�B�=B�<BЗB�MB�?B�YB�QB�kBۦB�xBܬB��B��B�B��B��B��B��B��B��B��B�B��B�B�$B�B�B�<B�.B�}B	�B	SB	YB	_B	_B		RB		lB	�B	
�B	�B	�B	�B	�B	�B	�B	�B	#B	�B	!�B	#B	#B	$@B	'RB	-CB	-]B	1[B	3hB	3hB	4�B	5tB	5�B	7�B	9�B	:�B	>�B	A�B	D�B	EB	HKB	OB	Q�B	RB	R B	QB	R:B	SB	SB	S&B	TB	VB	WYB	X_B	\CB	^OB	^OB	]IB	]dB	\�B	`vB	`�B	b�B	d�B	dtB	e�B	f�B	e�B	d�B	d�B	l�B	o�B	o�B	o�B	n�B	o�B	s�B	u�B	v�B	x�B	x�B	y�B	y�B	x�B	x�B	zB	{B	z�B	|6B	}B	}"B	.B	�AB	�-B	�3B	�SB	�gB	�zB	�rB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�2B	�>B	�B	�"B	�)B	�CB	�cB	�aB	�TB	�TB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	żB	żB	��B	��B	��B	��B	��B	�B	�B	�"B	�HB	�B	�B	�,B	�MB	�$B	�1B	�KB	�7B	�=B	�=B	�CB	�]B	�jB	�OB	�;B	�;B	�;B	�pB	�VB	�VB	�\B	�\B	�\B	�|B	�bB	�|B	�B	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�B	�B	�"B	�<B
 B
 4B
B
'B
;B
'B
'B
GB
-B
3B
MB
gB
�B
_B
fB
	RB
	lB
	�B

�B
xB
xB
~B
�B
pB
pB
\B
�B
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
"�B
#�B
$�B
$�B
%B
$�B
%,B
&2B
(
B
($B
(
B
'�B
(
B
($B
($B
(
B
'8B
(
B
(>B
($B
'�B
($B
(
B
(>B
($B
(�B
)B
)B
*0B
+B
+6B
,"B
,=B
-CB
.B
.B
-)B
-CB
.IB
/5B
0;B
0;B
1AB
2GB
49B
5ZB
6`B
7fB
7fB
6zB
7�B
8lB
8lB
8�B
8lB
8lB
8lB
7�B
7�B
9XB
9XB
9rB
9rB
8�B
8�B
7�B
7�B
8lB
9rB
:^B
:xB
9rB
9�B
9rB
9�B
:xB
;B
<�B
=�B
=qB
=�B
=�B
=�B
>wB
>�B
>�B
?�B
@�B
@�B
@�B
A�B
A�B
B�B
A�B
B�B
B�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
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
N�B
OB
N�B
N�B
N�B
N�B
O�B
Q B
P�B
Q B
R B
RB
R B
S&B
SB
R�B
TB
S�B
TB
TB
S�B
SB
S&B
T,B
T�B
U2B
UB
UB
VB
VB
W
B
VB
W$B
X+B
X+B
YB
YKB
Y1B
Y1B
YKB
Z7B
Z7B
ZQB
[=B
[=B
\)B
\CB
\)B
\CB
]/B
]IB
^5B
^OB
^5B
^5B
^5B
^OB
^OB
^OB
]dB
^jB
_VB
abB
aHB
abB
`\B
`\B
`\B
aHB
abB
aHB
abB
abB
aHB
aHB
abB
abB
abB
aHB
abB
abB
abB
bNB
bhB
bhB
bhB
bhB
bhB
dtB
dZB
dZB
dtB
dZB
dtB
dtB
c�B
ffB
f�B
f�B
gmB
gmB
gmB
g�B
hsB
hsB
hsB
h�B
h�B
h�B
hsB
hsB
iyB
iyB
iyB
iyB
iyB
i�B
i�B
jB
jB
jB
j�B
j�B
jB
jB
jB
jB
jB
jB
j�B
j�B
j�B
k�B
k�B
k�B
j�B
k�B
k�B
l�B
l�B
m�B
l�B
m�B
n�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709260032502017092600325020170926003250201806221231062018062212310620180622123106201804050426212018040504262120180405042621  JA  ARFMdecpA19c                                                                20170922063506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170921213507  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170921213509  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170921213509  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170921213510  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170921213510  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170921213510  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170921213510  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170921213511  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170921213511                      G�O�G�O�G�O�                JA  ARUP                                                                        20170921215508                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170924155549  CV  JULD            G�O�G�O�F�?�                JM  ARCAJMQC2.0                                                                 20170925153250  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170925153250  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192621  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033106  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                
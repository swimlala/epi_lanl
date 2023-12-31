CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-08T21:35:18Z creation;2016-10-08T21:35:21Z conversion to V3.1;2019-12-19T08:28:24Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20161008213518  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               -A   JA  I2_0576_045                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @���L�X�1   @�������@;���v�d�2�W��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D}��D~� D  D� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D��3D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�C3D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B =qB'�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C\C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�
D~}qD~�qD}qD�qD�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�D���D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�A�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�{�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ZA�ZA�XA�S�A�Q�A�A�AԅA��AӶFAӗ�A�x�A�ffA�(�A��A��A��A��;A���A�ƨAҧ�A�jA�`BA�1A�1'A��A�?}Aå�A�A���A��^A��A��^A�A�A�l�A���A��RA��yA�O�A�=qA���A��PA��HA�5?A�9XA��;A��A��A�bA�33A�|�A�/A���A��`A�-A�7LA��DA��yA�C�A���A���A���A���A�p�A�+A��;A�S�A���A�M�A�A��HA��uA�&�A��A���A}��A|�HA|~�A|  A{O�AzȴAz(�Ay��AyK�Ax  AxAwt�Avr�Au�Atr�As�#ArȴAq��Ap��Apz�Ao�FAo%AnQ�AlM�Ak�
AjĜAi��Ai
=Ah��Ahr�Ag��Afv�AehsAd$�Ac��Ac"�AbjAb{AaƨA`�+A^��A^5?A]7LA\$�AZ��AZ1AY��AYhsAY�AX�uAXM�AWO�AV^5AU�PAUVAT�AS��AS`BAR�uAR{AQ�AP�APM�AO��AO+AN��AMx�ALbNAK�AJ�yAI�AI"�AH�RAH�AG�AGoAF�uAE�AE�AE?}AC�^ABZA@�A?+A?%A=�FA=��A<n�A;�A:�RA:^5A:A9�^A9dZA8�DA7��A7\)A6�A6A4~�A2��A2�RA2bNA1oA/�
A.��A.1A-/A,��A,ZA+�A+`BA*�A*�A)�A(�A(ZA'��A&��A&v�A&{A%p�A$A"ȴA"�A ��A ��A ��A $�A�A��A�`A�
AC�A�`A��AS�A�DA�A�AoA~�A�A�A��A��A\)A��A{AS�A�A�#A
-A��A�
A~�A�
A��AA�!AVAQ�AQ�AE�AbAO�An�A�A �A ffA J@�K�@��R@��T@�p�@��u@�=q@� �@�ff@��/@�  @�!@�7L@��@���@�h@���@��@�=q@�V@�l�@�
=@��;@柾@�/@�r�@ܼj@܋D@ݲ-@�V@۝�@ڏ\@�V@ׅ@�x�@��@ԋD@Ӯ@�ȴ@Л�@�
=@��@�`B@ȋD@�ƨ@�l�@�+@���@Ǯ@�l�@�"�@�@���@�-@��T@ř�@ă@�9X@öF@§�@��h@�v�@��^@��9@�|�@�o@�
=@���@��@���@�M�@�X@�;d@�X@�1'@�;d@�ȴ@�n�@�@���@���@��@���@�r�@�(�@��@�+@�v�@�M�@��T@��`@�1'@��P@���@���@���@��@��@�v�@�@���@��@�%@��/@��@�I�@�1@���@�o@��@���@�x�@�7L@���@��@��@��@�l�@��H@��R@�=q@���@�X@�7L@�%@���@���@���@�=q@��@���@�j@��@�;d@�+@�"�@�
=@���@��\@�V@�{@�@��@�?}@���@�  @��m@�S�@�+@�@���@���@�M�@��h@�7L@��@���@�z�@� �@��@��
@�S�@��H@��\@�-@���@���@�&�@��j@��D@�b@��
@�ƨ@���@�;d@�K�@�ƨ@�9X@�Q�@� �@���@�K�@�
=@���@��R@��R@���@��\@��+@�v�@�n�@�ff@�-@��@��@���@���@���@��@}V@|I�@|j@|z�@|j@|9X@|�@|j@|j@|9X@{��@zM�@z�@y��@x��@x�@xbN@xbN@xbN@xQ�@xA�@x �@xb@x1'@xA�@x1'@x �@x �@w�;@w|�@vff@v{@v@u�T@u@u�h@t�@t�@tZ@t�@s�
@t1@tI�@tj@t�@uV@t�@t�@uV@u`B@u/@t��@tZ@s��@s�m@s�
@s�@s@rM�@qx�@p�@pQ�@pQ�@pr�@p�@o�;@o�P@oK�@oK�@o�@n�y@n�@n�@n�@n�+@nV@n@m@m`B@mO�@m/@l�@l��@l�j@l�j@l�D@l9X@l1@k�F@kdZ@ko@j��@j=q@i�#@ix�@iX@i7L@h�`@hĜ@hbN@h  @g��@g
=@f�R@f��@f$�@e�-@e�h@e/@d�@d�/@d�@dz�@c�@c@c@b�H@b��@b��@b-@a��@ax�@a&�@a%@`�`@`�9@`�@`A�@_�@_��@_K�@^�R@^E�@]�T@]`B@]/@]V@\�/@\�D@\9X@[��@[t�@["�@["�@[o@[@Z~�@Z-@ZJ@Y�#@YG�@X��@X�u@X1'@W�@W�@W\)@V�y@V�y@V�R@VE�@V$�@V$�@U��@U?}@T��@S�F@S"�@R�H@R��@R=q@QX@Q�@P�`@P�`@P�9@P �@O\)@N�y@NE�@M��@M�@M�@L�j@L�@L�@L��@Lz�@L9X@L(�@Kƨ@J�@J�H@J~�@J=q@I�@IX@H��@H�9@Hr�@G��@Gl�@GK�@G+@Fȴ@F��@E@EO�@EV@D�D@C��@C33@B��@B�\@B~�@BM�@BM�@B-@A��@AX@@��@@��@@1'@?�@?
=@>��@>ff@>V@>{@=`B@=V@<�@<�j@<Z@<(�@;�F@;dZ@;33@:��@:�\@:M�@:�@9��@9hs@9G�@9�@8Ĝ@8Ĝ@8�9@8��@8�u@8Q�@81'@7�w@7��@7|�@7;d@6�R@6ff@6E�@65?@6$�@6@5��@5@5�-@5��@5�@5p�@5O�@5�@4��@4��@4�@4��@4I�@3�m@3��@3dZ@2�H@2�\@2~�@2M�@2=q@1��@1hs@1G�@17L@17L@1&�@0��@0�@01'@/�@/�;@/�w@/�@.��@.�y@.�@.ȴ@.E�@-�-@-`B@-O�@-?}@-/@,�/@,Z@,1@+��@+��@+C�@+o@+@*�H@*��@*��@*��@*~�@*n�@*M�@*-@)�@)��@)G�@(�u@(�@(r�@(bN@( �@'�@'�@'�;@'|�@'+@&�+@&E�@&5?@&$�@&$�@&{@&@&@&@&@&@&@&@&{@&{@&{@%�@%`B@$z�@#ƨ@#��@#��@#S�@#@"�@"��@"n�@"�@!��@!�^@!G�@ ��@ �@ Q�@ A�@ 1'@  �@   @|�@K�@ȴ@v�@@��@`B@/@V@�@�@j@(�@(�@(�@�@ƨ@t�@@��@M�@=q@-@x�@7L@�@��@bN@�@�w@�P@l�@\)@K�@;d@;d@+@��@�y@�y@�@��@�+@ff@{@�h@`B@/@V@��@�@�D@9X@��@�F@�@dZ@S�@S�@C�@"�@�H@��@��@��@��@M�@=q@��@�^@�7@X@7L@�`@�@bN@Q�@1'@ �@  @�@��@�P@+@ȴ@��@��@V@E�@5?@{@��@�-@�@/@��@��@I�@�
@��@t�@S�@C�@33@o@
��@
��@
��@
�\@
~�@
M�@
�@
�@	��@	�@	��@	x�@	hs@	7L@��@��@�9@�u@r�@A�@b@�@�;@�w@�P@�P@�P@|�@l�@\)@K�@�@��@�R@��@v�@E�@{@{@$�@5?@5?@5?@5?@{@$�@{@E�@V@V@$�@�T@�@@��@��@�h@`B@O�@?}@��@�/@�j@�@z�@Z@I�@I�@�@�m@��@t�@t�@33@o@o@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ZA�ZA�XA�S�A�Q�A�A�AԅA��AӶFAӗ�A�x�A�ffA�(�A��A��A��A��;A���A�ƨAҧ�A�jA�`BA�1A�1'A��A�?}Aå�A�A���A��^A��A��^A�A�A�l�A���A��RA��yA�O�A�=qA���A��PA��HA�5?A�9XA��;A��A��A�bA�33A�|�A�/A���A��`A�-A�7LA��DA��yA�C�A���A���A���A���A�p�A�+A��;A�S�A���A�M�A�A��HA��uA�&�A��A���A}��A|�HA|~�A|  A{O�AzȴAz(�Ay��AyK�Ax  AxAwt�Avr�Au�Atr�As�#ArȴAq��Ap��Apz�Ao�FAo%AnQ�AlM�Ak�
AjĜAi��Ai
=Ah��Ahr�Ag��Afv�AehsAd$�Ac��Ac"�AbjAb{AaƨA`�+A^��A^5?A]7LA\$�AZ��AZ1AY��AYhsAY�AX�uAXM�AWO�AV^5AU�PAUVAT�AS��AS`BAR�uAR{AQ�AP�APM�AO��AO+AN��AMx�ALbNAK�AJ�yAI�AI"�AH�RAH�AG�AGoAF�uAE�AE�AE?}AC�^ABZA@�A?+A?%A=�FA=��A<n�A;�A:�RA:^5A:A9�^A9dZA8�DA7��A7\)A6�A6A4~�A2��A2�RA2bNA1oA/�
A.��A.1A-/A,��A,ZA+�A+`BA*�A*�A)�A(�A(ZA'��A&��A&v�A&{A%p�A$A"ȴA"�A ��A ��A ��A $�A�A��A�`A�
AC�A�`A��AS�A�DA�A�AoA~�A�A�A��A��A\)A��A{AS�A�A�#A
-A��A�
A~�A�
A��AA�!AVAQ�AQ�AE�AbAO�An�A�A �A ffA J@�K�@��R@��T@�p�@��u@�=q@� �@�ff@��/@�  @�!@�7L@��@���@�h@���@��@�=q@�V@�l�@�
=@��;@柾@�/@�r�@ܼj@܋D@ݲ-@�V@۝�@ڏ\@�V@ׅ@�x�@��@ԋD@Ӯ@�ȴ@Л�@�
=@��@�`B@ȋD@�ƨ@�l�@�+@���@Ǯ@�l�@�"�@�@���@�-@��T@ř�@ă@�9X@öF@§�@��h@�v�@��^@��9@�|�@�o@�
=@���@��@���@�M�@�X@�;d@�X@�1'@�;d@�ȴ@�n�@�@���@���@��@���@�r�@�(�@��@�+@�v�@�M�@��T@��`@�1'@��P@���@���@���@��@��@�v�@�@���@��@�%@��/@��@�I�@�1@���@�o@��@���@�x�@�7L@���@��@��@��@�l�@��H@��R@�=q@���@�X@�7L@�%@���@���@���@�=q@��@���@�j@��@�;d@�+@�"�@�
=@���@��\@�V@�{@�@��@�?}@���@�  @��m@�S�@�+@�@���@���@�M�@��h@�7L@��@���@�z�@� �@��@��
@�S�@��H@��\@�-@���@���@�&�@��j@��D@�b@��
@�ƨ@���@�;d@�K�@�ƨ@�9X@�Q�@� �@���@�K�@�
=@���@��R@��R@���@��\@��+@�v�@�n�@�ff@�-@��@��@���@���@���@��@}V@|I�@|j@|z�@|j@|9X@|�@|j@|j@|9X@{��@zM�@z�@y��@x��@x�@xbN@xbN@xbN@xQ�@xA�@x �@xb@x1'@xA�@x1'@x �@x �@w�;@w|�@vff@v{@v@u�T@u@u�h@t�@t�@tZ@t�@s�
@t1@tI�@tj@t�@uV@t�@t�@uV@u`B@u/@t��@tZ@s��@s�m@s�
@s�@s@rM�@qx�@p�@pQ�@pQ�@pr�@p�@o�;@o�P@oK�@oK�@o�@n�y@n�@n�@n�@n�+@nV@n@m@m`B@mO�@m/@l�@l��@l�j@l�j@l�D@l9X@l1@k�F@kdZ@ko@j��@j=q@i�#@ix�@iX@i7L@h�`@hĜ@hbN@h  @g��@g
=@f�R@f��@f$�@e�-@e�h@e/@d�@d�/@d�@dz�@c�@c@c@b�H@b��@b��@b-@a��@ax�@a&�@a%@`�`@`�9@`�@`A�@_�@_��@_K�@^�R@^E�@]�T@]`B@]/@]V@\�/@\�D@\9X@[��@[t�@["�@["�@[o@[@Z~�@Z-@ZJ@Y�#@YG�@X��@X�u@X1'@W�@W�@W\)@V�y@V�y@V�R@VE�@V$�@V$�@U��@U?}@T��@S�F@S"�@R�H@R��@R=q@QX@Q�@P�`@P�`@P�9@P �@O\)@N�y@NE�@M��@M�@M�@L�j@L�@L�@L��@Lz�@L9X@L(�@Kƨ@J�@J�H@J~�@J=q@I�@IX@H��@H�9@Hr�@G��@Gl�@GK�@G+@Fȴ@F��@E@EO�@EV@D�D@C��@C33@B��@B�\@B~�@BM�@BM�@B-@A��@AX@@��@@��@@1'@?�@?
=@>��@>ff@>V@>{@=`B@=V@<�@<�j@<Z@<(�@;�F@;dZ@;33@:��@:�\@:M�@:�@9��@9hs@9G�@9�@8Ĝ@8Ĝ@8�9@8��@8�u@8Q�@81'@7�w@7��@7|�@7;d@6�R@6ff@6E�@65?@6$�@6@5��@5@5�-@5��@5�@5p�@5O�@5�@4��@4��@4�@4��@4I�@3�m@3��@3dZ@2�H@2�\@2~�@2M�@2=q@1��@1hs@1G�@17L@17L@1&�@0��@0�@01'@/�@/�;@/�w@/�@.��@.�y@.�@.ȴ@.E�@-�-@-`B@-O�@-?}@-/@,�/@,Z@,1@+��@+��@+C�@+o@+@*�H@*��@*��@*��@*~�@*n�@*M�@*-@)�@)��@)G�@(�u@(�@(r�@(bN@( �@'�@'�@'�;@'|�@'+@&�+@&E�@&5?@&$�@&$�@&{@&@&@&@&@&@&@&@&{@&{@&{@%�@%`B@$z�@#ƨ@#��@#��@#S�@#@"�@"��@"n�@"�@!��@!�^@!G�@ ��@ �@ Q�@ A�@ 1'@  �@   @|�@K�@ȴ@v�@@��@`B@/@V@�@�@j@(�@(�@(�@�@ƨ@t�@@��@M�@=q@-@x�@7L@�@��@bN@�@�w@�P@l�@\)@K�@;d@;d@+@��@�y@�y@�@��@�+@ff@{@�h@`B@/@V@��@�@�D@9X@��@�F@�@dZ@S�@S�@C�@"�@�H@��@��@��@��@M�@=q@��@�^@�7@X@7L@�`@�@bN@Q�@1'@ �@  @�@��@�P@+@ȴ@��@��@V@E�@5?@{@��@�-@�@/@��@��@I�@�
@��@t�@S�@C�@33@o@
��@
��@
��@
�\@
~�@
M�@
�@
�@	��@	�@	��@	x�@	hs@	7L@��@��@�9@�u@r�@A�@b@�@�;@�w@�P@�P@�P@|�@l�@\)@K�@�@��@�R@��@v�@E�@{@{@$�@5?@5?@5?@5?@{@$�@{@E�@V@V@$�@�T@�@@��@��@�h@`B@O�@?}@��@�/@�j@�@z�@Z@I�@I�@�@�m@��@t�@t�@33@o@o@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�qB�wB�wB��BĜBŢBɺB��B��B��B��B��B��BƨB��B��B��B��B��B��B��B��B��B�jB�VBQ�B(�B��B�sB�5B�BB��B�\B�VB��B�oBiyBM�BI�B33B"�B�BVB+B%B  B�BBȴB�XB��B��B�uB�=Bz�Bn�BR�B33B&�B�B
��B
�`B
�B
��B
��B
ƨB
�}B
�LB
�'B
�B
�B
��B
��B
�hB
z�B
t�B
q�B
l�B
dZB
`BB
\)B
W
B
VB
P�B
T�B
Q�B
K�B
D�B
:^B
5?B
2-B
-B
-B
,B
(�B
"�B
"�B
�B
hB
JB
B	��B	��B	��B	��B	�B	�fB	�/B	�B	�B	��B	��B	��B	ɺB	�wB	�XB	�9B	�B	��B	��B	��B	��B	��B	�uB	�hB	�PB	�1B	�B	|�B	w�B	z�B	x�B	u�B	u�B	o�B	n�B	m�B	l�B	gmB	e`B	^5B	R�B	N�B	E�B	;dB	33B	-B	)�B	)�B	)�B	+B	-B	1'B	1'B	/B	%�B	�B	B	B��B	B��B��B��B��B��B��B��B�B�B�B�B�B�ZB�B�5B�;B�)B�
B��B��B��B��BȴBƨBĜB��B�wB�dB�RB�?B�-B�B�B�B�B��B��B��B��B��B��B��B�bB�+B�B{�B|�Bz�By�Bu�Bk�BffBcTBbNBaHB`BB]/B[#BYBW
BVBS�BR�BP�BO�BJ�BH�BE�BA�B?}B?}B>wB=qB<jB<jB<jB;dB;dB:^B8RB6FB5?B49B33B2-B2-B1'B/B.B.B+B)�B'�B&�B%�B$�B"�B!�B �B�B �B�B�B�B�B)�B+B'�B�BoB�B'�B+B+B+B,B)�B)�B,B-B-B,B+B(�B&�B"�B �B�B�B �B+B-B2-B33B33B33B5?B49B49B33B6FB9XB;dB<jB<jB9XB5?B2-B1'B2-B0!B0!B0!B+B'�B#�B#�B%�B&�B'�B'�B'�B'�B,B0!B1'B2-B33B33B49B6FB6FB7LB:^B<jB?}B@�BB�BB�BH�BL�BQ�BQ�BR�BVBZB[#B[#B`BBcTBdZBffBgmBn�Bp�Br�Bt�Bt�Bt�Bv�Bw�Bx�Bx�By�By�By�By�Bz�B~�B�B�%B�%B�=B�JB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�9B�FB�^BBĜBĜBĜBƨB��B��B��B�
B�#B�BB�ZB�`B�B�B��B��B��B��B��B	  B	+B	JB	bB	{B	�B	�B	�B	 �B	"�B	#�B	#�B	"�B	#�B	#�B	$�B	$�B	%�B	(�B	)�B	/B	49B	5?B	9XB	:^B	:^B	A�B	G�B	I�B	J�B	K�B	M�B	R�B	XB	XB	YB	[#B	[#B	\)B	^5B	_;B	_;B	`BB	aHB	bNB	cTB	dZB	hsB	k�B	m�B	m�B	n�B	n�B	p�B	p�B	r�B	q�B	r�B	r�B	s�B	t�B	u�B	u�B	v�B	y�B	{�B	~�B	�B	�B	�B	�7B	�DB	�JB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�-B	�3B	�3B	�3B	�3B	�3B	�?B	�?B	�?B	�LB	�RB	�XB	�^B	�jB	�jB	�qB	�qB	�wB	�}B	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�NB	�TB	�TB	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
+B
+B
+B
+B
+B
	7B

=B
DB
PB
PB
VB
\B
\B
\B
\B
bB
bB
bB
bB
hB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
$�B
$�B
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
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
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
6FB
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
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
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
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
B�B
C�B
C�B
B�B
D�B
D�B
E�B
E�B
E�B
D�B
E�B
E�B
D�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
G�B
H�B
H�B
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
P�B
P�B
P�B
P�B
P�B
P�B
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
VB
VB
VB
VB
VB
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
^5B
^5B
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
aHB
bNB
bNB
bNB
cTB
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
ffB
ffB
ffB
ffB
gmB
gmB
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
iyB
iyB
iyB
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
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
m�B
m�B
n�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B�B�B��B�PB�.B�.B�.B�\B�0BƨB��B��B�B�B�[B�hB�~B҉BϫBĶB��B[�B1�B��B�QB��B�B�xB��B��B��B��B��Bq�BR:BM�B6�B$�B	BB�B	lBB�B̘B��B�CB��B�gB�~B}<Br-BV9B4�B)DB�BAB
�B
��B
��B
�B
ǮB
��B
��B
��B
��B
�)B
�FB
��B
��B
|B
utB
raB
m]B
eB
`�B
\�B
W�B
W$B
QNB
U�B
S&B
MB
E�B
;dB
6�B
3�B
-�B
-�B
-B
)�B
$&B
$�B
�B
�B
jB
�B	��B	�qB	�B	�lB	�B	�B	��B	��B	��B	ԕB	ҽB	�}B	�^B	��B	��B	��B	��B	��B	�;B	�	B	�B	�EB	�,B	��B	��B	�7B	��B	}�B	xlB	{�B	y�B	v�B	v�B	p;B	o5B	nIB	m]B	hXB	gB	_�B	S�B	PB	F�B	<PB	3�B	-�B	*�B	*�B	*�B	+�B	-�B	1�B	3B	1B	'�B	7B	�B	aB�rB	uB��B�<B�jB�jB�^B��B��B��B�9B��B�B�B��B��B�!B��B��BؓB��B��B�jB�xB�lB�zB�mBªB�cB�PB�	B�B��B��B��B�IB��B�fB��B��B�	B�KB��B�+B� B�lB�[B|�B}�B{�B|Bx�BlWBgBd&Bc:BbhBa|B]�B\CBY�BW�BW$BT�BS�BR�BRBL~BJrBGEBB[B@ B@OB>�B=�B<�B<�B<�B<B<�B;�B9rB7LB6+B4�B3�B2�B2�B1�B0!B/�B/�B,=B+B(�B(
B'B%�B#�B"�B!|B �B!�B�B�B�B�B+6B,�B*�B�B�BB(�B,B+�B,"B-)B+6B*eB,�B-�B./B-�B,�B+kB(
B#�B!HB BB �B+QB-]B2aB3hB3�B3�B5�B4�B4�B3�B6�B:DB<�B>(B="B:*B6B2�B1vB2�B1B1�B1'B,B)yB%,B$�B&�B'RB(XB(sB(�B)DB-CB0�B1�B2|B3�B3�B4�B6�B6�B8B:�B=B@OBA BCBC�BIlBM6BRTBRTBS[BV9BZQB[qB[qB`�Bc�Bd�Bf�Bh>Bn�Bp�Br�BuBuBu%BwLBx8By>By>BzDBz*BzBz*B{JB.B�-B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�NB�fB�$B�eB�cB�OB�[B�hB��B��B��B��B��B��B��B��B��B�JB�oB�sB�qB��B�B��B��B��B�B�B�B�	B�B��B	�B	B	}B	�B	�B	�B	�B	!B	"�B	#�B	#�B	"�B	#�B	$B	$�B	$�B	&B	)DB	*B	/OB	4nB	5�B	:*B	;0B	:�B	A�B	G�B	I�B	J�B	K�B	M�B	SB	X+B	X_B	YB	[=B	[WB	\xB	^OB	_VB	_;B	`\B	abB	bhB	cnB	dtB	h�B	k�B	m�B	m�B	n�B	n�B	p�B	p�B	r�B	q�B	r�B	r�B	s�B	t�B	u�B	u�B	v�B	y�B	{�B	~�B	�B	��B	�B	�7B	�DB	�JB	�VB	�}B	��B	��B	��B	��B	��B	��B	��B	�!B	�B	� B	��B	��B	��B	�B	�WB	�)B	�5B	�-B	�GB	�hB	�hB	�3B	�MB	�hB	�ZB	�ZB	�tB	��B	��B	�rB	�xB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�.B	� B	�&B	�,B	�SB	�$B	�+B	�1B	�7B	�QB	�QB	�qB	�]B	�IB	�OB	�OB	�OB	ߊB	�vB	�B	�nB	�B	�zB	�zB	�zB	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�"B	�(B	�.B
 iB
GB
MB
3B
gB
mB
EB
_B
_B
EB
zB
	�B

�B
xB
�B
�B
�B
vB
\B
\B
vB
}B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"B
#B
$B
$�B
$�B
%�B
&B
&2B
'B
'B
($B
(>B
(
B
)*B
)B
*B
*B
*0B
*B
+6B
+6B
,=B
,"B
,"B
-CB
-B
-B
-)B
-CB
-)B
./B
.cB
/5B
/5B
/OB
/OB
0UB
1AB
1'B
1AB
1AB
1AB
1'B
1AB
2-B
2GB
2GB
2GB
2aB
2aB
3MB
3MB
3hB
3hB
4TB
4TB
4TB
5tB
5ZB
6`B
6`B
6zB
6`B
6zB
7fB
7LB
7fB
7fB
7fB
8�B
8lB
9�B
9rB
9�B
9�B
:xB
:^B
:xB
:xB
:�B
;�B
<�B
<jB
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?}B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
B�B
C�B
C�B
B�B
D�B
D�B
E�B
E�B
E�B
D�B
E�B
E�B
D�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
G�B
H�B
H�B
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
NB
M�B
NB
N�B
OB
OB
N�B
O�B
O�B
PB
O�B
O�B
O�B
Q B
P�B
Q B
QB
Q B
Q4B
RB
RB
S&B
SB
S@B
TB
TB
TB
T,B
UB
UB
UB
UB
VB
VB
VB
VB
VB
VB
VB
VB
V9B
VB
W$B
W$B
W$B
W?B
X+B
X+B
X+B
X+B
X+B
X_B
YKB
Y1B
Z7B
Z7B
Z7B
[#B
[#B
[=B
[=B
[=B
[#B
[=B
\)B
\CB
\CB
\]B
\CB
]dB
]IB
]IB
]IB
]dB
]IB
^OB
^5B
^OB
^5B
^jB
^OB
^jB
^OB
_VB
_pB
`vB
`\B
`\B
`BB
aHB
abB
abB
abB
abB
abB
b�B
b�B
b�B
cnB
cnB
cnB
d�B
dZB
dtB
dtB
dtB
dtB
e`B
e`B
ezB
ezB
ezB
e`B
ffB
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
hsB
hsB
h�B
h�B
hsB
iyB
iyB
iyB
iyB
i�B
i�B
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
m�B
m�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
m�B
m�B
n�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<9#�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610180050372016101800503720161018005037201806221215062018062212150620180622121506201804050407532018040504075320180405040753  JA  ARFMdecpA19c                                                                20161009063506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161008213518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161008213519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161008213519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161008213520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161008213520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161008213520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161008213520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161008213521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161008213521                      G�O�G�O�G�O�                JA  ARUP                                                                        20161008223208                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161009153725  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161017155037  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161017155037  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190753  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031506  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                
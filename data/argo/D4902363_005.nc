CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-24T06:44:26Z creation;2016-06-24T06:44:27Z conversion to V3.1;2019-12-19T08:38:42Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160624064426  20200115101517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_005                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ײ�"�� 1   @ײ���� @;���w�k�dehr� �1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�ɚD�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG�)CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!��D!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�;�D�{�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��RD�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��PA��A��A��+A��+A��A��A��A��A��A��A��A�\)A��wA���A�ffA��
A�bNA��9A�
=A�bA�
=A�x�A�(�A��yA�&�A��DA��hA��
A�|�A��`A�G�A�v�A���A��7A�5?A��;A�^5A���A�`BA��wA���A���A���A�9XA�%A��A�ffA��A���A�G�A�C�A�ĜA�ĜA��-A�Q�A��PA���A��A��A��A�ffA��HA��/A��yA��+A�jA�"�A���A���A���A��`A�ZA���A���A���A��A�v�A�ȴA��+A�JA�O�A�ƨA�XA�K�A�%A��7A�A�A��TA��
A�|�A�A���A�G�A�|�A�ƨA�&�A��DA��uA�XA�dZAXA~�9A~��A~�A}`BA{VAy�-Ay&�Ax�RAwhsAv�At��Ar�yAqK�Aop�An��Am��Al�uAl�Akx�AjZAi�Ag�TAg�Af�Ae/Ad��Ac?}AbA`�RA_+A^�jA^�9A^^5A]t�A]VA\�A\�A[|�AZ��AYdZAX �AW��AW/AVJAT9XAS��AS�PASt�ASK�AS�AQ��AQ"�AP�!AO�AO"�AMx�AL�jALffAK��AKx�AJ��AJM�AI\)AG�AE��AC�FAB��AAt�A?��A>E�A<��A<A�A;p�A;/A:$�A9"�A8jA6��A6-A5�A5�^A5G�A4ĜA41'A2bNA1C�A1"�A0�jA0n�A/p�A.1A-�A,Q�A+�A*�DA)�A(�`A(�A'��A'7LA&�!A&�+A&A�A&1A%p�A$�`A#�A!��A!
=A �jA ~�A $�A�TA\)A33AoA9XAƨA�A1A/A$�Ap�AoA�HA�Ax�A�A�HA9XA��AdZAG�A%A�jA$�AdZA"�A�HA��AE�A;dA�jA|�A	�FAVA;dAbNA �A�mAXA��A�DA��A;dA��A�AffA�A�hAoA �!@���@�r�@���@���@��@���@��`@@��y@�M�@�t�@�=q@��@�@�M�@�O�@䛦@�I�@���@㕁@�\@�|�@ޟ�@�dZ@��;@�x�@��T@���@�z�@�I�@��;@ϥ�@θR@͡�@�&�@̛�@��@ˍP@�o@��y@��#@���@� �@�ȴ@�%@�K�@�{@�p�@���@��@��h@��D@���@��
@���@�|�@��#@�z�@���@��@�~�@��^@��@��/@��u@��@���@��`@��u@�Z@�I�@� �@��@���@���@�O�@��@��@�n�@��T@�`B@�9X@��
@���@���@���@��@��H@�ff@�l�@�V@��@���@�\)@��@�ȴ@��R@���@��+@�V@�5?@��#@���@�x�@�7L@�bN@�  @�;d@��@�C�@�+@�5?@���@�7L@���@�ȴ@��!@��!@���@���@���@���@���@��+@�~�@�v�@�n�@�ff@�v�@�V@�-@��h@���@��@��F@�K�@�@��!@�v�@��@���@��@�x�@�hs@��@���@�1'@��F@���@�l�@��@�$�@��#@��@�?}@�V@��`@�(�@���@�l�@��H@��R@�^5@��@���@�%@��j@�r�@�1'@�1@���@��w@���@�\)@�
=@���@��@��R@��\@�E�@�$�@���@��^@���@��@�Z@���@�K�@�+@��@���@��y@���@���@�~�@�ff@�E�@��@��@�@��7@�O�@�7L@���@�A�@�@~�y@~�+@~@}�T@}@}�h@}?}@|z�@|1@{��@{C�@z��@z�\@z�\@z~�@z~�@zJ@y&�@x�`@xĜ@xĜ@xQ�@x  @w�;@w�@v�y@up�@tZ@s��@s�@so@q�^@qG�@q7L@q&�@q%@pĜ@p�u@pQ�@o�w@oK�@n�+@n{@m@m�@l�@l�@l�j@l�D@lj@lI�@l�@ko@j~�@jM�@j-@i�@i��@ihs@iG�@h��@h��@g��@fȴ@f��@f��@e��@e`B@d�/@d9X@c"�@b��@b�!@b�!@b��@bn�@b-@a�^@ax�@a&�@`Ĝ@`�@`Q�@`1'@`  @_��@_�w@_�@_��@_�P@_;d@^�@^ȴ@^��@^v�@^V@^E�@^$�@]�T@]?}@\I�@[33@[S�@[S�@[33@Z�@Zn�@ZM�@Z=q@ZM�@ZM�@ZJ@Y��@Y�#@Y�@Y�@Y�@Y�@Y��@Y7L@Y�@X��@X��@XbN@W��@W\)@WK�@W�@V��@Vȴ@V�R@V��@V��@V��@Vȴ@Vȴ@Vȴ@Vȴ@VV@U��@U��@Up�@UO�@T�j@Tz�@Tz�@T(�@S�m@S��@S"�@R��@R��@R��@R��@R�!@R��@R~�@RJ@Q�^@Q�7@QG�@P��@Pr�@O��@O\)@N�y@N�y@N�y@N�@N�R@Nv�@Nff@Nff@NV@N5?@N$�@N$�@N{@M�@M�@M�T@M@M�h@M�@MV@L�/@L�j@L�@KdZ@KC�@K"�@K"�@K@J�!@J�\@J�@I��@H��@Hb@G\)@F�@Fv�@F5?@F@E��@E�-@E�@E?}@D��@D�@D9X@C�
@CdZ@Co@B��@B-@A�^@A�7@Ahs@AX@A�@@��@@��@@�`@@Ĝ@@�9@@�9@@��@@�u@@r�@@A�@?�w@?l�@>ȴ@>��@>V@>5?@>5?@>$�@>$�@>{@>@=��@=�h@=V@<z�@<�@;��@;t�@;C�@;33@;o@:�H@:�!@:=q@9hs@97L@9�@8�9@8�9@8�9@8�@8b@7�@7K�@6��@6V@6$�@6@5��@5�@4�@3��@3�m@3�F@3�@3t�@3t�@3dZ@3C�@3o@2��@2�!@2��@2~�@2=q@2J@1x�@1%@0�9@0bN@01'@0b@/�;@/�;@/|�@/+@.�@.�R@.��@.�+@.E�@.{@.@-��@-��@-�@-`B@-�@,�@,�@,I�@+�
@+��@+dZ@*M�@)�7@)�@(��@(�9@(r�@(Q�@( �@'�@'��@'�@'��@'l�@'
=@&��@&�y@&�@&�R@&��@&�+@&v�@&ff@&E�@&@%@%��@%�@%p�@%`B@%?}@%/@%�@$�@$�j@$�@$z�@$�@#�F@#��@#��@#�@#S�@#C�@#o@#@"�@"��@"��@"~�@"M�@"�@!�#@!�^@!�^@!��@!�7@!hs@!�@!%@ �`@ ��@ Ĝ@ �u@ �@ �@ r�@ bN@ Q�@  �@�;@�@l�@K�@+@��@��@V@�T@�h@�@�h@�@p�@?}@/@�@�/@�j@�@j@9X@1@��@�m@�
@��@dZ@33@@��@-@�#@x�@G�@7L@�@�`@Ĝ@�9@��@�@�w@l�@\)@\)@K�@+@
=@��@�@ȴ@ȴ@�R@��@v�@ff@5?@�-@�@?}@��@j@I�@�F@��@��@��@S�@�\@M�@�@��@��@�@��@��@�^@��@��@��@X@&�@Ĝ@Ĝ@r�@Q�@1'@  @�@�;@�;@��@�w@��@|�@l�@l�@\)@�@�y@�y@ȴ@��@��@��@��@v�@V@5?@5?@$�@$�@5?@5?@$�@$�@$�@@��@�@`B@?}@/@��@�@��@�j@��@j@I�@9X@9X@(�@1@�m@ƨ@ƨ@�F@�F@�F@��@�@33@@
�H@
^5@
M�@
M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��PA��A��A��+A��+A��A��A��A��A��A��A��A�\)A��wA���A�ffA��
A�bNA��9A�
=A�bA�
=A�x�A�(�A��yA�&�A��DA��hA��
A�|�A��`A�G�A�v�A���A��7A�5?A��;A�^5A���A�`BA��wA���A���A���A�9XA�%A��A�ffA��A���A�G�A�C�A�ĜA�ĜA��-A�Q�A��PA���A��A��A��A�ffA��HA��/A��yA��+A�jA�"�A���A���A���A��`A�ZA���A���A���A��A�v�A�ȴA��+A�JA�O�A�ƨA�XA�K�A�%A��7A�A�A��TA��
A�|�A�A���A�G�A�|�A�ƨA�&�A��DA��uA�XA�dZAXA~�9A~��A~�A}`BA{VAy�-Ay&�Ax�RAwhsAv�At��Ar�yAqK�Aop�An��Am��Al�uAl�Akx�AjZAi�Ag�TAg�Af�Ae/Ad��Ac?}AbA`�RA_+A^�jA^�9A^^5A]t�A]VA\�A\�A[|�AZ��AYdZAX �AW��AW/AVJAT9XAS��AS�PASt�ASK�AS�AQ��AQ"�AP�!AO�AO"�AMx�AL�jALffAK��AKx�AJ��AJM�AI\)AG�AE��AC�FAB��AAt�A?��A>E�A<��A<A�A;p�A;/A:$�A9"�A8jA6��A6-A5�A5�^A5G�A4ĜA41'A2bNA1C�A1"�A0�jA0n�A/p�A.1A-�A,Q�A+�A*�DA)�A(�`A(�A'��A'7LA&�!A&�+A&A�A&1A%p�A$�`A#�A!��A!
=A �jA ~�A $�A�TA\)A33AoA9XAƨA�A1A/A$�Ap�AoA�HA�Ax�A�A�HA9XA��AdZAG�A%A�jA$�AdZA"�A�HA��AE�A;dA�jA|�A	�FAVA;dAbNA �A�mAXA��A�DA��A;dA��A�AffA�A�hAoA �!@���@�r�@���@���@��@���@��`@@��y@�M�@�t�@�=q@��@�@�M�@�O�@䛦@�I�@���@㕁@�\@�|�@ޟ�@�dZ@��;@�x�@��T@���@�z�@�I�@��;@ϥ�@θR@͡�@�&�@̛�@��@ˍP@�o@��y@��#@���@� �@�ȴ@�%@�K�@�{@�p�@���@��@��h@��D@���@��
@���@�|�@��#@�z�@���@��@�~�@��^@��@��/@��u@��@���@��`@��u@�Z@�I�@� �@��@���@���@�O�@��@��@�n�@��T@�`B@�9X@��
@���@���@���@��@��H@�ff@�l�@�V@��@���@�\)@��@�ȴ@��R@���@��+@�V@�5?@��#@���@�x�@�7L@�bN@�  @�;d@��@�C�@�+@�5?@���@�7L@���@�ȴ@��!@��!@���@���@���@���@���@��+@�~�@�v�@�n�@�ff@�v�@�V@�-@��h@���@��@��F@�K�@�@��!@�v�@��@���@��@�x�@�hs@��@���@�1'@��F@���@�l�@��@�$�@��#@��@�?}@�V@��`@�(�@���@�l�@��H@��R@�^5@��@���@�%@��j@�r�@�1'@�1@���@��w@���@�\)@�
=@���@��@��R@��\@�E�@�$�@���@��^@���@��@�Z@���@�K�@�+@��@���@��y@���@���@�~�@�ff@�E�@��@��@�@��7@�O�@�7L@���@�A�@�@~�y@~�+@~@}�T@}@}�h@}?}@|z�@|1@{��@{C�@z��@z�\@z�\@z~�@z~�@zJ@y&�@x�`@xĜ@xĜ@xQ�@x  @w�;@w�@v�y@up�@tZ@s��@s�@so@q�^@qG�@q7L@q&�@q%@pĜ@p�u@pQ�@o�w@oK�@n�+@n{@m@m�@l�@l�@l�j@l�D@lj@lI�@l�@ko@j~�@jM�@j-@i�@i��@ihs@iG�@h��@h��@g��@fȴ@f��@f��@e��@e`B@d�/@d9X@c"�@b��@b�!@b�!@b��@bn�@b-@a�^@ax�@a&�@`Ĝ@`�@`Q�@`1'@`  @_��@_�w@_�@_��@_�P@_;d@^�@^ȴ@^��@^v�@^V@^E�@^$�@]�T@]?}@\I�@[33@[S�@[S�@[33@Z�@Zn�@ZM�@Z=q@ZM�@ZM�@ZJ@Y��@Y�#@Y�@Y�@Y�@Y�@Y��@Y7L@Y�@X��@X��@XbN@W��@W\)@WK�@W�@V��@Vȴ@V�R@V��@V��@V��@Vȴ@Vȴ@Vȴ@Vȴ@VV@U��@U��@Up�@UO�@T�j@Tz�@Tz�@T(�@S�m@S��@S"�@R��@R��@R��@R��@R�!@R��@R~�@RJ@Q�^@Q�7@QG�@P��@Pr�@O��@O\)@N�y@N�y@N�y@N�@N�R@Nv�@Nff@Nff@NV@N5?@N$�@N$�@N{@M�@M�@M�T@M@M�h@M�@MV@L�/@L�j@L�@KdZ@KC�@K"�@K"�@K@J�!@J�\@J�@I��@H��@Hb@G\)@F�@Fv�@F5?@F@E��@E�-@E�@E?}@D��@D�@D9X@C�
@CdZ@Co@B��@B-@A�^@A�7@Ahs@AX@A�@@��@@��@@�`@@Ĝ@@�9@@�9@@��@@�u@@r�@@A�@?�w@?l�@>ȴ@>��@>V@>5?@>5?@>$�@>$�@>{@>@=��@=�h@=V@<z�@<�@;��@;t�@;C�@;33@;o@:�H@:�!@:=q@9hs@97L@9�@8�9@8�9@8�9@8�@8b@7�@7K�@6��@6V@6$�@6@5��@5�@4�@3��@3�m@3�F@3�@3t�@3t�@3dZ@3C�@3o@2��@2�!@2��@2~�@2=q@2J@1x�@1%@0�9@0bN@01'@0b@/�;@/�;@/|�@/+@.�@.�R@.��@.�+@.E�@.{@.@-��@-��@-�@-`B@-�@,�@,�@,I�@+�
@+��@+dZ@*M�@)�7@)�@(��@(�9@(r�@(Q�@( �@'�@'��@'�@'��@'l�@'
=@&��@&�y@&�@&�R@&��@&�+@&v�@&ff@&E�@&@%@%��@%�@%p�@%`B@%?}@%/@%�@$�@$�j@$�@$z�@$�@#�F@#��@#��@#�@#S�@#C�@#o@#@"�@"��@"��@"~�@"M�@"�@!�#@!�^@!�^@!��@!�7@!hs@!�@!%@ �`@ ��@ Ĝ@ �u@ �@ �@ r�@ bN@ Q�@  �@�;@�@l�@K�@+@��@��@V@�T@�h@�@�h@�@p�@?}@/@�@�/@�j@�@j@9X@1@��@�m@�
@��@dZ@33@@��@-@�#@x�@G�@7L@�@�`@Ĝ@�9@��@�@�w@l�@\)@\)@K�@+@
=@��@�@ȴ@ȴ@�R@��@v�@ff@5?@�-@�@?}@��@j@I�@�F@��@��@��@S�@�\@M�@�@��@��@�@��@��@�^@��@��@��@X@&�@Ĝ@Ĝ@r�@Q�@1'@  @�@�;@�;@��@�w@��@|�@l�@l�@\)@�@�y@�y@ȴ@��@��@��@��@v�@V@5?@5?@$�@$�@5?@5?@$�@$�@$�@@��@�@`B@?}@/@��@�@��@�j@��@j@I�@9X@9X@(�@1@�m@ƨ@ƨ@�F@�F@�F@��@�@33@@
�H@
^5@
M�@
M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B9XB9XB9XB9XB8RB9XB9XB9XB8RB8RB8RB=qBB�BK�BgmBk�Bm�Bm�Bm�Bm�Bn�Bl�Bm�BW
BD�B?}B5?B+B%�B"�B �B�BuB\BJB1BB��B�B��B��B��B��B��B��B��B�B�?B��B�PB�BffB[#B�B��B��B��B��B�{B�+Bz�Bx�Bo�B� Bu�BW
B8RB7LB9XBB�B49B7LB.B$�B{BbB1B��B�B�B�NB��BŢB�^B�LB�9B�B��B{�BgmBN�BA�B:^B33B#�B{B1B
��B
�`B
��B
��B
��B
��B
��B
��B
�oB
�B
p�B
hsB
aHB
S�B
D�B
=qB
0!B
%�B
�B
�B
�B
�B
\B
%B	��B	�B	�sB	�BB	�BB	�
B	��B	ȴB	��B	�dB	�-B	�-B	�9B	�?B	�'B	�!B	�B	�B	��B	��B	��B	�hB	�\B	�DB	�B	}�B	w�B	v�B	u�B	t�B	s�B	q�B	jB	iyB	ffB	bNB	^5B	YB	XB	T�B	S�B	P�B	L�B	E�B	9XB	$�B	\B	%B��B�B�B�ZB�BB�/B�#B�B��B��B��B��BɺBȴBƨBÖB��B�}B�qB�wB�jB�dB�RB�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�PB�PB�JB�JB�JB�JB�JB�VB�DB�JB�+B�B}�B|�Bz�By�Bx�Bx�Bp�Bm�BjBhsBgmBffBe`BcTBbNB^5B^5B]/B[#BW
BR�BP�BL�BE�BB�B?}B>wB=qB<jB;dB:^B9XB8RB7LB6FB5?B5?B49B33B2-B0!B/B/B+B)�B'�B%�B%�B#�B"�B!�B"�B �B�B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B#�B#�B#�B#�B&�B)�B+B,B,B,B,B1'B2-B49B49B5?B7LB8RB8RB8RB<jB=qB>wB?}B?}B?}B?}B?}BA�BC�BE�BG�BI�BP�BQ�BS�B[#B]/B^5BbNBdZBdZBe`BbNBffBiyBl�Bn�Bo�Bq�Bq�Bq�Br�Br�Bs�Bs�Bu�Bv�Bv�Bw�Bz�B|�B� B�B�B�=B�\B�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�9B�LB�RB�XB�XB�XB�jB�}B��BĜBƨBɺB��B��B��B�B�B�B�#B�;B�TB�ZB�sB�yB�B�B�B�B�B��B��B��B��B��B��B��B	  B	  B	B	B	B	B	B	B	B	%B	
=B	\B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	%�B	%�B	'�B	'�B	(�B	+B	-B	0!B	1'B	2-B	33B	5?B	7LB	8RB	9XB	:^B	=qB	>wB	?}B	?}B	A�B	E�B	I�B	J�B	K�B	J�B	L�B	N�B	O�B	P�B	Q�B	XB	\)B	]/B	_;B	`BB	hsB	k�B	l�B	m�B	o�B	p�B	q�B	r�B	s�B	u�B	w�B	y�B	z�B	}�B	� B	�B	�B	�B	�B	�%B	�%B	�DB	�VB	�\B	�\B	�hB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�3B	�9B	�?B	�FB	�RB	�RB	�^B	�dB	�qB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�#B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�`B	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
DB
DB
DB
DB
JB
PB
VB
\B
\B
\B
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
,B
,B
+B
+B
+B
+B
+B
+B
,B
-B
.B
.B
/B
/B
0!B
0!B
1'B
2-B
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
5?B
5?B
6FB
7LB
7LB
8RB
9XB
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
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
A�B
B�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
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
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
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
R�B
S�B
S�B
S�B
S�B
S�B
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
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
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
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B9rB9XB9XB9XB8RB9XB9XB9XB8RB8lB8�B>wBE9BQBm�BpBn�Bo5BoiBpBqvBpoBrBY�BF�BAB7LB,�B&�B$@B"NBCB�BbBB	7BtB��B�'BϑB�-B�yB�B��B�CB��B��B�fB��B�pB��BgmB[�B�{B��B�VB�B��B�mB��B|By�Bp�B�[Bx�BY1B9>B8B:BC�B4�B8�B/OB&�B�B�B	�B�VB��B�B��B� B�tB��B�RB�B�B��B~�BjBPBB�B;B5B%�BB
#B
�}B
�B
ϑB
��B
�qB
�B
��B
��B
��B
�{B
qvB
i_B
b�B
U�B
FtB
?�B
2-B
'�B
�B
�B
B
?B
}B
�B	�jB	�B	�_B	�-B	�B	�B	ҽB	�XB	�-B	��B	��B	�|B	��B	�FB	��B	��B	�B	� B	�0B	�fB	�B	� B	�}B	��B	��B	~wB	x8B	v�B	vFB	uZB	u%B	r|B	kQB	j�B	g�B	dB	_;B	Y�B	X�B	U�B	T�B	R B	NVB	G�B	<B	'mB	 B	�B�.B�tB�B�zB�bB�BܒB�qB�SB��BΊB�)B�=BɆBǮB��BÖB��B��B�B�<B��B��B��B�CB�$B�B��B�B��B�]B�EB�+B��B�
B�
B��B��B��B�qB�[B��B��B��B��B�B��B��B��B�BB�dB��B�fB�aB~�B}qB{dBz�Bz�Bz�Bq�Bn}BkBh�Bg�Bf�Be�Bd@Bc:B^�B_!B^�B]BXyBTBR�BO(BGzBC�B@�B>�B>B=<B<B;B:xB8�B7�B6�B5�B5�B5B4B33B1�B1[B1B-B+�B)DB'RB&�B$�B#�B#�B#�B!bB!B!B!|B;B!BOB5B�B�BB�B�BkB�B]BBB)BB]BdBB5B5B5B!BB�B �B �B �B"B#B$�B$tB$�B%B'�B*�B+kB,=B,WB,�B-CB2-B2�B4�B4�B5�B7�B8�B8�B9�B=<B=�B>�B?�B?�B?�B@ B@4BBABDBF?BH�BJ�BQhBRoBT�B[qB]IB^jBb�Bd�Bd�BfLBdZBg�BjKBl�Bn�BpBq�Bq�Bq�Br�Br�Bs�BtBu�Bv�Bw2BxlB{JB}qB�4B�B�mB��B��B�B�{B�#B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�HB�TB�mB��B�]B�iB��B��B��B��B�lB��B��B��B��B� B�B��B��B�=B�bB�FB�MB�_B�KB�kB��B߾B�B��B��B��B��B��B�B��B��B��B��B�B�*B�6B�<B�(B	 4B	 4B	;B	UB	;B	AB	aB	SB	mB	�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	 B	!�B	#B	&B	&LB	(>B	(>B	)_B	+6B	-]B	0;B	1[B	2GB	3�B	5�B	7�B	8�B	9�B	:�B	=�B	>�B	?�B	?�B	A�B	E�B	I�B	J�B	K�B	J�B	MB	N�B	P.B	QNB	R�B	X_B	\]B	]dB	_�B	`�B	h�B	k�B	l�B	m�B	o�B	p�B	q�B	r�B	tB	vB	xB	zB	{B	~B	�B	�'B	�3B	�9B	�9B	�YB	��B	�xB	�pB	�vB	�vB	��B	��B	��B	��B	��B	��B	�	B	��B	��B	�2B	�0B	�eB	�QB	�wB	�;B	�AB	�MB	�MB	�TB	�tB	�zB	�lB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	ĶB	ĶB	żB	żB	��B	��B	�B	�DB	�B	��B	��B	�B	�B	�B	�B	� B	��B	��B	�B	�B	�
B	�1B	�B	�#B	�WB	�=B	�WB	�OB	�pB	ߊB	�\B	�vB	�B	�hB	�B	�B	�nB	�zB	�zB	�fB	�mB	�B	�yB	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�	B	��B	�B	�B	�"B	�(B
 4B
 B
B
B
AB
'B
-B
-B
B
GB
MB
3B
B
MB
SB
B
9B
9B
9B
tB
EB
_B
_B
�B
	�B
^B
^B
DB
^B
dB
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 B
 �B
!�B
"�B
#B
"�B
"�B
"�B
"�B
"�B
#�B
$B
$&B
%B
&B
'B
($B
)DB
)B
)B
)*B
*0B
*0B
*KB
+QB
,=B
,"B
+B
+B
+B
+6B
+QB
+QB
,=B
-CB
./B
./B
/OB
/5B
0UB
0oB
1[B
2GB
2GB
3MB
33B
33B
3MB
3hB
3MB
4TB
4TB
4TB
5tB
5ZB
5tB
6zB
7�B
7fB
8�B
9rB
9rB
9rB
9rB
9�B
:�B
;B
;B
;�B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
BB
B�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
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
L�B
L�B
L�B
NB
NB
M�B
M�B
N�B
N�B
OB
N�B
OB
O�B
O�B
PB
O�B
PB
Q B
Q B
QB
Q B
P�B
P�B
Q B
Q B
QB
RB
RB
RB
SB
SB
SB
S&B
T,B
T,B
TB
S�B
S�B
SB
TB
TB
S�B
TB
TB
UB
UB
UB
VB
VB
VB
VB
V9B
W$B
W$B
W?B
X+B
XEB
X_B
Y1B
Y1B
Z7B
ZB
Z7B
Z7B
Z7B
Z7B
[WB
[=B
[WB
\]B
\)B
\)B
\CB
\CB
\]B
\)B
\CB
]IB
]/B
]/B
]dB
]IB
]IB
]IB
]dB
^OB
^OB
^jB
_VB
_VB
_pB
_pB
_;B
_VB
_pB
`vB
a|B
abB
abB
aHB
abB
abB
bNB
bNB
bNB
bhB
bhB
bhB
bhB
bhB
cTB
cnB
cnB
c�B
cnB
dZB
dZB
dZB
dZB
dtB
dtB
d�B
e`B
e`B
ezB
ezB
e�B
e`B
e�B
ezB
ffB
ffB
ffB
f�B
f�B
ffB
ffB
ffB
ffB
ffB
f�B
f�B
ffB
ffB
f�B
f�B
g�B
g�B
g�B
g�B
h�B
hsB
h�B
hsB
h�B
h�B
h�B
h�B
iyB
iyB
i�B
i�B
i�B
iyB
iyB
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606150033392016061500333920160615003339201806221142572018062211425720180622114257201804050401342018040504013420180405040134  JA  ARFMdecpA19c                                                                20160624153525  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160624064426  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160624064426  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160624064426  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160624064427  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160624064427  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160624064427  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160624064427  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20160624064427                      G�O�G�O�G�O�                JA  ARUP                                                                        20160624071802                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160611153541  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20160614153339  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160614153339  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190134  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622024257  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101517                      G�O�G�O�G�O�                
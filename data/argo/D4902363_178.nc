CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-11T21:35:08Z creation;2017-11-11T21:35:12Z conversion to V3.1;2019-12-19T07:56:56Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20171111213508  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_178                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�4���� 1   @�4�@y] @;z�L�_�dh�u1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�<�Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6w
D6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�;�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�{�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bA�bA�oA�bA�bA�bA�VA�%A���Aǡ�A��AƇ+A�`BA�E�A��A���A�ƨAŮAř�AŅA�l�A�5?A���A�x�A�jA�`BA�E�A��A��`A��#A���A�(�A��A��HA��mA��jA�l�A�;dA�^5A�hsA�bA�^5A�{A�bNA�C�A�O�A��yA�A��hA���A�5?A�t�A��A���A�G�A�+A��;A���A�ffA���A��/A��7A�&�A�&�A�l�A��
A��A�\)A��DA���A��-A���A��A�+A�+A���A�hsA��
A��A��A�  A���A�1'A���A��PA�bA~��A}�TA|�yA{�hAz�!Ay�FAyt�Ax�AxbAwC�Av�!Avn�Av=qAu�wAu33At��As��Ar�uAr(�Aq+Aox�AnĜAn�\AnI�Amx�Al�HAlbNAk��Aj��Ajv�AjjAj1'Ai?}Ah(�AgXAf�AfQ�Ae�Ad��Ac��Ab�Ab$�AbAaK�A`�jA]�PA[��AZM�AY��AX�AW�AW7LAVĜAVffAV{AUx�AT1'AR��AR1AQ�7APr�AO�AN�\AM�-AMK�AK?}AIoAHbNAF�HAE|�AE;dAD�`AD9XAC�7AB�AA�wAAl�A@�yA@A�A?�PA>�A=ƨA<��A<�\A:�\A:1A9�;A9�7A9
=A7��A6��A5x�A4��A45?A2n�A0��A0bNA0�A/�PA/O�A,v�A*�RA*�A)A)�A)
=A(��A(~�A(5?A'�wA'C�A&�HA&=qA%�;A$�A#��A"ĜA"�A!�hA ��A�A�yA1'AA��A�`A��A�A��A
=A��AM�A^5A��Ax�AM�A+A�FA5?A�A�RA��A�\A�A�A/A��A�AQ�A1'A��A�
AhsAjA+A
��A
=qA	�-A	VA�9A
=A\)A1'A��A^5A33@���@�Z@���@�x�@��@��9@�A�@��m@��H@�~�@��-@�Ĝ@�@�=q@���@�%@�I�@�R@��@� �@�|�@�v�@�{@�/@�Q�@�P@�+@�R@�\@�@��@��@��/@��D@�;d@܃@��@�5?@�{@ٺ^@�X@ׅ@���@�E�@�/@�ƨ@θR@�M�@͑h@��@̃@�X@���@�A�@�1@Õ�@�5?@���@�7L@�A�@�v�@��@��@�b@�S�@��\@�$�@�`B@��@���@��P@��@�?}@�A�@�S�@�
=@��@��u@�@��H@�M�@��u@�~�@�=q@��@��-@�7L@��y@�p�@��9@��u@�j@�Q�@�A�@�1@��w@��P@�t�@�\)@�C�@��y@���@�o@���@�V@�{@�X@�b@�ƨ@���@�|�@�t�@�\)@�"�@��H@�E�@��-@�`B@��@��j@�z�@��@���@�V@�x�@�&�@�&�@�/@��@��F@�@���@�@�@���@��@��+@��@��+@�`B@�I�@��;@��@�C�@��H@���@�V@���@��/@��@���@�z�@�Z@�9X@�1'@� �@��
@��@�S�@�"�@��H@�5?@��T@�O�@��@���@��P@��R@�33@���@�O�@�|�@��\@��@�+@���@��w@���@���@�%@�7L@��@��7@��7@�O�@�G�@���@�1@���@���@���@��@�;d@�C�@��@�V@���@���@���@�j@�1@��
@��@�dZ@��!@�O�@�A�@+@~�R@~�+@~5?@}?}@|��@|9X@|9X@|(�@{C�@z�H@{C�@y�@w�@w+@w
=@v�@u�T@u�h@up�@u`B@u`B@u?}@t��@t9X@sS�@s"�@r�@r��@r��@r��@r�!@r-@q�7@qx�@q�@p��@p�@pQ�@pQ�@p1'@p �@pb@o�@o�@m�T@m��@m`B@m/@l(�@l�@k�m@k��@kt�@k33@j��@j�\@jn�@j=q@jJ@ihs@h�`@h��@h�@hQ�@h �@h �@h �@g�@g�;@g��@g��@g;d@g;d@gK�@g;d@g+@g�@f��@f�+@f$�@e��@e�@e`B@d��@c��@cS�@c33@b�H@bM�@aX@`Ĝ@`��@`��@`�u@`�@`  @_�w@_��@_��@^��@^5?@]�T@]O�@\�@\�D@\(�@[�m@[�F@[C�@Z�@Z��@Z�!@Z�\@Z^5@Y��@Y�^@Y��@Yx�@Yhs@YX@Y&�@Y�@[o@Z~�@Y�^@Y��@Y��@Yhs@Y�@Y%@X�9@W�w@Wl�@W;d@W
=@Vff@U�T@U��@UV@T�D@T�@Sƨ@S�F@S�m@S�
@St�@SS�@SC�@SC�@S33@R�@R=q@Q��@Q�^@Q��@Qx�@QX@Q��@Q��@Qhs@Q�@Q%@P�`@P�9@P �@O��@O+@N�+@N5?@M@Mp�@M`B@M?}@M�@L��@KS�@J�@I��@I�@I�@JJ@JJ@I�#@H��@Hb@G�w@G�@Gl�@G�@Fȴ@Fff@F@E��@D�/@DZ@D�@C��@C�m@Cƨ@C��@CdZ@Co@B�H@B�!@B��@B�\@B�!@B-@A�7@A&�@@�`@@�u@@Q�@@b@?��@?K�@?+@>�R@>E�@=@=�@=p�@=p�@<��@<z�@<(�@:�!@:�@9��@9��@9x�@9&�@8Q�@7�w@7�P@7l�@6��@6�R@6��@6E�@6E�@6E�@6{@5��@5@5��@5p�@5/@4��@4(�@41@3��@3S�@3"�@2�H@2�\@1�^@17L@0��@0��@0�@0r�@0bN@0 �@/|�@/;d@.��@.v�@.V@.$�@-��@-�h@-�@-`B@-/@-/@-�@,�@,9X@+�
@+�@+dZ@+dZ@+dZ@+dZ@+dZ@+o@*��@*~�@*M�@*-@*J@*J@*J@*J@*J@*-@*=q@*=q@*-@*�@*�@)�^@)&�@)%@(Ĝ@(�u@(r�@(A�@(b@'�;@'�@'|�@'l�@&��@&�@&�@&�@&�@&�@&�@&ȴ@&��@&E�@&@%�@%p�@%p�@%`B@%?}@%�@$��@$I�@$(�@#�m@#��@#��@#��@#��@#�@#t�@#o@"�H@"��@"��@"��@"�!@"��@"~�@"=q@!��@!��@!X@!�@ Ĝ@ �@ Q�@ A�@ 1'@ b@�P@K�@�@�@E�@��@/@�/@�@z�@9X@(�@1@ƨ@��@t�@33@@��@�!@�!@��@n�@-@�@�#@��@hs@�`@�`@��@ �@�@�P@l�@;d@;d@;d@+@��@5?@{@�T@�h@?}@�j@z�@z�@j@j@j@j@j@j@Z@Z@Z@9X@�
@�@t�@t�@t�@t�@t�@t�@S�@�@^5@��@��@x�@x�@x�@x�@x�@7L@�@�9@�@r�@Q�@1'@ �@b@b@�@�@|�@;d@�@��@ff@$�@�@�h@�@�@�@�@�@�@O�@/@��@��@�D@Z@�@ƨ@��@�@dZ@S�@33@
�@
��@
�!@
��@
��@
��@
^5@
�@	�@	��@	x�@	G�@	&�@��@�`@�`@��@��@�u@�@bN@�;@��@|�@\)@;d@�@�@�R@�+@v�@ff@ff@E�@$�@@�T@��@`B@`B@O�@?}@/@�@�@��@�j@�@(�@�m@�
@ƨ@��@C�@33@"�@o@o@o@@@�@�H@�!@�!@�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bA�bA�oA�bA�bA�bA�VA�%A���Aǡ�A��AƇ+A�`BA�E�A��A���A�ƨAŮAř�AŅA�l�A�5?A���A�x�A�jA�`BA�E�A��A��`A��#A���A�(�A��A��HA��mA��jA�l�A�;dA�^5A�hsA�bA�^5A�{A�bNA�C�A�O�A��yA�A��hA���A�5?A�t�A��A���A�G�A�+A��;A���A�ffA���A��/A��7A�&�A�&�A�l�A��
A��A�\)A��DA���A��-A���A��A�+A�+A���A�hsA��
A��A��A�  A���A�1'A���A��PA�bA~��A}�TA|�yA{�hAz�!Ay�FAyt�Ax�AxbAwC�Av�!Avn�Av=qAu�wAu33At��As��Ar�uAr(�Aq+Aox�AnĜAn�\AnI�Amx�Al�HAlbNAk��Aj��Ajv�AjjAj1'Ai?}Ah(�AgXAf�AfQ�Ae�Ad��Ac��Ab�Ab$�AbAaK�A`�jA]�PA[��AZM�AY��AX�AW�AW7LAVĜAVffAV{AUx�AT1'AR��AR1AQ�7APr�AO�AN�\AM�-AMK�AK?}AIoAHbNAF�HAE|�AE;dAD�`AD9XAC�7AB�AA�wAAl�A@�yA@A�A?�PA>�A=ƨA<��A<�\A:�\A:1A9�;A9�7A9
=A7��A6��A5x�A4��A45?A2n�A0��A0bNA0�A/�PA/O�A,v�A*�RA*�A)A)�A)
=A(��A(~�A(5?A'�wA'C�A&�HA&=qA%�;A$�A#��A"ĜA"�A!�hA ��A�A�yA1'AA��A�`A��A�A��A
=A��AM�A^5A��Ax�AM�A+A�FA5?A�A�RA��A�\A�A�A/A��A�AQ�A1'A��A�
AhsAjA+A
��A
=qA	�-A	VA�9A
=A\)A1'A��A^5A33@���@�Z@���@�x�@��@��9@�A�@��m@��H@�~�@��-@�Ĝ@�@�=q@���@�%@�I�@�R@��@� �@�|�@�v�@�{@�/@�Q�@�P@�+@�R@�\@�@��@��@��/@��D@�;d@܃@��@�5?@�{@ٺ^@�X@ׅ@���@�E�@�/@�ƨ@θR@�M�@͑h@��@̃@�X@���@�A�@�1@Õ�@�5?@���@�7L@�A�@�v�@��@��@�b@�S�@��\@�$�@�`B@��@���@��P@��@�?}@�A�@�S�@�
=@��@��u@�@��H@�M�@��u@�~�@�=q@��@��-@�7L@��y@�p�@��9@��u@�j@�Q�@�A�@�1@��w@��P@�t�@�\)@�C�@��y@���@�o@���@�V@�{@�X@�b@�ƨ@���@�|�@�t�@�\)@�"�@��H@�E�@��-@�`B@��@��j@�z�@��@���@�V@�x�@�&�@�&�@�/@��@��F@�@���@�@�@���@��@��+@��@��+@�`B@�I�@��;@��@�C�@��H@���@�V@���@��/@��@���@�z�@�Z@�9X@�1'@� �@��
@��@�S�@�"�@��H@�5?@��T@�O�@��@���@��P@��R@�33@���@�O�@�|�@��\@��@�+@���@��w@���@���@�%@�7L@��@��7@��7@�O�@�G�@���@�1@���@���@���@��@�;d@�C�@��@�V@���@���@���@�j@�1@��
@��@�dZ@��!@�O�@�A�@+@~�R@~�+@~5?@}?}@|��@|9X@|9X@|(�@{C�@z�H@{C�@y�@w�@w+@w
=@v�@u�T@u�h@up�@u`B@u`B@u?}@t��@t9X@sS�@s"�@r�@r��@r��@r��@r�!@r-@q�7@qx�@q�@p��@p�@pQ�@pQ�@p1'@p �@pb@o�@o�@m�T@m��@m`B@m/@l(�@l�@k�m@k��@kt�@k33@j��@j�\@jn�@j=q@jJ@ihs@h�`@h��@h�@hQ�@h �@h �@h �@g�@g�;@g��@g��@g;d@g;d@gK�@g;d@g+@g�@f��@f�+@f$�@e��@e�@e`B@d��@c��@cS�@c33@b�H@bM�@aX@`Ĝ@`��@`��@`�u@`�@`  @_�w@_��@_��@^��@^5?@]�T@]O�@\�@\�D@\(�@[�m@[�F@[C�@Z�@Z��@Z�!@Z�\@Z^5@Y��@Y�^@Y��@Yx�@Yhs@YX@Y&�@Y�@[o@Z~�@Y�^@Y��@Y��@Yhs@Y�@Y%@X�9@W�w@Wl�@W;d@W
=@Vff@U�T@U��@UV@T�D@T�@Sƨ@S�F@S�m@S�
@St�@SS�@SC�@SC�@S33@R�@R=q@Q��@Q�^@Q��@Qx�@QX@Q��@Q��@Qhs@Q�@Q%@P�`@P�9@P �@O��@O+@N�+@N5?@M@Mp�@M`B@M?}@M�@L��@KS�@J�@I��@I�@I�@JJ@JJ@I�#@H��@Hb@G�w@G�@Gl�@G�@Fȴ@Fff@F@E��@D�/@DZ@D�@C��@C�m@Cƨ@C��@CdZ@Co@B�H@B�!@B��@B�\@B�!@B-@A�7@A&�@@�`@@�u@@Q�@@b@?��@?K�@?+@>�R@>E�@=@=�@=p�@=p�@<��@<z�@<(�@:�!@:�@9��@9��@9x�@9&�@8Q�@7�w@7�P@7l�@6��@6�R@6��@6E�@6E�@6E�@6{@5��@5@5��@5p�@5/@4��@4(�@41@3��@3S�@3"�@2�H@2�\@1�^@17L@0��@0��@0�@0r�@0bN@0 �@/|�@/;d@.��@.v�@.V@.$�@-��@-�h@-�@-`B@-/@-/@-�@,�@,9X@+�
@+�@+dZ@+dZ@+dZ@+dZ@+dZ@+o@*��@*~�@*M�@*-@*J@*J@*J@*J@*J@*-@*=q@*=q@*-@*�@*�@)�^@)&�@)%@(Ĝ@(�u@(r�@(A�@(b@'�;@'�@'|�@'l�@&��@&�@&�@&�@&�@&�@&�@&ȴ@&��@&E�@&@%�@%p�@%p�@%`B@%?}@%�@$��@$I�@$(�@#�m@#��@#��@#��@#��@#�@#t�@#o@"�H@"��@"��@"��@"�!@"��@"~�@"=q@!��@!��@!X@!�@ Ĝ@ �@ Q�@ A�@ 1'@ b@�P@K�@�@�@E�@��@/@�/@�@z�@9X@(�@1@ƨ@��@t�@33@@��@�!@�!@��@n�@-@�@�#@��@hs@�`@�`@��@ �@�@�P@l�@;d@;d@;d@+@��@5?@{@�T@�h@?}@�j@z�@z�@j@j@j@j@j@j@Z@Z@Z@9X@�
@�@t�@t�@t�@t�@t�@t�@S�@�@^5@��@��@x�@x�@x�@x�@x�@7L@�@�9@�@r�@Q�@1'@ �@b@b@�@�@|�@;d@�@��@ff@$�@�@�h@�@�@�@�@�@�@O�@/@��@��@�D@Z@�@ƨ@��@�@dZ@S�@33@
�@
��@
�!@
��@
��@
��@
^5@
�@	�@	��@	x�@	G�@	&�@��@�`@�`@��@��@�u@�@bN@�;@��@|�@\)@;d@�@�@�R@�+@v�@ff@ff@E�@$�@@�T@��@`B@`B@O�@?}@/@�@�@��@�j@�@(�@�m@�
@ƨ@��@C�@33@"�@o@o@o@@@�@�H@�!@�!@�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��BɺBɺBǮBǮB��B�/B�NB�`B�fB�mB�mB�fB�`B�ZB�ZB�B%B�B{BuBoBbBhBhBPB\B{BDB�B�RBF�BVB�B�BB�BÖB��BɺB�^B�B��B��B��B��B��B�\By�Bv�Bw�Bl�B]/BW
BO�BC�B33B)�B&�B�B�BbB
=BB
��B
��B
��B
�`B
ɺB
��B
ɺB
�}B
�FB
�'B
�B
��B
��B
��B
��B
�hB
�+B
�B
~�B
w�B
t�B
n�B
l�B
iyB
dZB
_;B
]/B
\)B
ZB
VB
P�B
K�B
F�B
=qB
:^B
33B
+B
&�B
%�B
"�B
�B
�B
�B
oB
VB
JB
JB
	7B
B	��B	��B	��B	�B	�B	�mB	�HB	�B	�B	�
B	��B	ȴB	�FB	�B	��B	��B	��B	��B	��B	��B	��B	�{B	�\B	�+B	~�B	{�B	y�B	r�B	l�B	gmB	aHB	]/B	P�B	C�B	E�B	=qB	9XB	;dB	9XB	49B	0!B	,B	'�B	'�B	#�B	�B	�B	�B	{B	bB	VB	B	B	B	B��B��B�B�sB�yB�ZB�B��B�B�
B��B��B��B�LB�}B�}B�wB�jB�dB�^B�RB�?B�3B�-B�B�B��B��B��B��B��B��B��B�hB�oB�oB�uB�bB�\B�\B�=B�1B�1B�Bx�B|�B{�Bs�Bn�BhsBdZBe`BdZBe`Be`BbNB_;B_;B]/B^5B]/B]/B[#BYBVBO�BI�BM�BL�BJ�BF�BD�B9XB49B7LB49B5?B.B#�B0!B1'B2-B49B49B33B1'B/B1'B.B,B%�B!�B/B-B,B(�B(�B+B-B,B/B-B.B0!B33B33B49B2-B33B33B33B2-B-B(�B-B.B1'B.B,B$�B�B(�B-B.B/B2-B2-B1'B0!B(�B!�B8RB9XB8RB49B8RB:^B8RB5?B>wB>wB@�BC�BD�BF�BF�BG�BF�BA�B=qBD�BC�BE�BH�BE�BE�BD�BM�BJ�BE�BF�BW
BW
BXBW
BR�BXB^5BdZBffBffBffBffBgmBjBk�Bl�Bl�Bk�Bn�Bu�Bx�B{�B{�By�Bx�B�B�B�B�B�B�B�B�B�B�B�+B�%B�1B�7B�bB��B��B��B��B��B��B��B��B�B�3B�LB�^B�dB�qB�}BBȴB��B�B�B�B�B�B�#B�B�B�BB�`B�yB�B�B�B�B�B��B��B��B	  B	  B	B	B	B	%B	B	B	%B	+B	B��B��B	+B	VB	hB	uB	�B	�B	�B	!�B	#�B	$�B	$�B	$�B	%�B	$�B	$�B	'�B	)�B	(�B	,B	1'B	2-B	2-B	1'B	2-B	1'B	49B	5?B	49B	6FB	6FB	6FB	49B	2-B	9XB	?}B	C�B	D�B	E�B	C�B	D�B	F�B	H�B	J�B	L�B	N�B	S�B	S�B	P�B	S�B	XB	XB	XB	[#B	]/B	^5B	^5B	]/B	]/B	_;B	aHB	gmB	k�B	m�B	n�B	n�B	n�B	n�B	p�B	v�B	u�B	w�B	w�B	w�B	y�B	z�B	{�B	{�B	|�B	|�B	� B	�+B	�+B	�7B	�JB	�VB	�\B	�bB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�-B	�?B	�FB	�RB	�XB	�dB	�}B	��B	��B	��B	B	ĜB	ŢB	ŢB	ŢB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�/B	�NB	�ZB	�ZB	�`B	�fB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
B
B
B
B
%B
B
B
B
%B
+B
+B
%B
%B
+B
1B
1B
1B

=B
JB
PB
PB
PB
PB
PB
PB
\B
hB
oB
oB
oB
oB
uB
uB
{B
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
$�B
&�B
%�B
&�B
'�B
'�B
'�B
'�B
)�B
+B
+B
,B
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
/B
/B
1'B
1'B
2-B
2-B
2-B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
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
6FB
7LB
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
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
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
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
L�B
L�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
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
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
W
B
W
B
W
B
W
B
VB
W
B
VB
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
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
^5B
_;B
_;B
_;B
^5B
^5B
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
cTB
cTB
dZB
cTB
cTB
dZB
dZB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
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
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
iyB
iyB
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
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B�#BȚB�BбB�~B�B�B�B�B�B�B�B�B��B�vB�B�B�B�B�B�B�B�BpBBMBB�TB�oBU�B�B"B�B�B��B�BуB�JB��B�'B��B��B�B��B�dB�@B~�By$By	Bo5B`BBYBRBF�B6FB,=B(�B!bB�B�B�BGB
��B
�qB
��B
�B
��B
ЗB
�)B
�B
�8B
�GB
�]B
��B
��B
��B
�_B
��B
��B
�B
�OB
yXB
u�B
o�B
m)B
jKB
e`B
`BB
]�B
\xB
Z�B
V�B
Q�B
L�B
G�B
>�B
;0B
4�B
,�B
'�B
&LB
#nB
�B
kB
EB
[B
BB
�B
�B
	�B
AB	�"B	��B	�`B	�vB	�wB	�sB	�B	یB	��B	�sB	�B	�=B	��B	�OB	��B	�
B	��B	�B	�=B	�7B	�
B	�B	�bB	��B	��B	|�B	z�B	tB	m�B	h�B	b�B	^OB	S�B	E�B	F�B	?HB	:�B	;�B	9�B	5?B	1'B	-]B	(�B	(sB	$�B	 �B	�B	�B	�B	�B	\B	%B	�B	�B	�B��B�`B��B�KB�eB��B�WB��BخBרB��B�B��B�XB�OB� B�B�B��B��B��B��B��B��B� B��B��B�-B��B��B�~B��B��B��B�[B�B��B�NB��B��B�^B�B��B�9B{B}�B|�ButBpUBjeBfBffBeFBe�Be�Bc B_�B_�B]�B^�B]~B]�B[�BY�BV�BQNBKxBN�BM�BK�BG�BE�B;�B6zB8�B5�B6zB0!B&�B1B2GB2�B4�B4�B3�B1�B/�B1�B/ B-)B($B#�B/iB-�B,�B*0B*0B+�B-�B,�B/�B-�B.�B0�B3�B3�B4nB2�B3�B3�B3�B2�B.IB*�B-�B.�B1[B.�B,�B&�B 'B)�B-�B/B/�B2�B2�B1�B1B+QB$tB8�B9�B8�B5?B8�B:�B9$B6zB>�B?.BA;BD3BESBGBG+BG�BGBB�B>wBE9BDgBFtBI7BF�BF�BE�BN"BKxBF�BG�BW?BWYBXyBW�BT�BX�B^�BdtBf�Bf�Bf�Bf�Bg�Bj�Bk�Bl�Bl�Bk�Bn�Bu�By>B|6B|PBz�By�B�UB�AB�3B�3B�gB�SB�gB��B�{B�mB��B��B��B��B� B�
B�=B��B�B�B�RB��B��B�6B�3B�LB�^B�JB�<B�}B�BɆBϑB�mB�yB�B�BچB�qBٚBںB�vB�zB�B�B�B�B��B�B�$B�B�.B	 OB	 �B	aB	�B	mB	�B	�B	�B	%B	�B	B�B��B	�B	"B	4B	[B	mB	eB	�B	!�B	#�B	$�B	$�B	%,B	&B	%FB	%`B	($B	*KB	)_B	,=B	1B	2aB	2|B	1�B	2�B	1�B	4�B	5tB	4�B	6zB	6zB	6�B	4�B	33B	:B	?�B	C�B	D�B	E�B	C�B	D�B	F�B	H�B	J�B	M6B	OB	TB	T{B	Q�B	TaB	X+B	XEB	X_B	[=B	]IB	^OB	^OB	]IB	]dB	_�B	a�B	g�B	k�B	m�B	n�B	n�B	n�B	n�B	p�B	v�B	u�B	w�B	w�B	w�B	y�B	z�B	|B	|B	}"B	}<B	��B	�+B	�_B	��B	��B	��B	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	�B	�,B	�B	�
B	�*B	�B	�B	�"B	�=B	�CB	�/B	�IB	�IB	�!B	�;B	�;B	�;B	�;B	�;B	�UB	�aB	�ZB	�zB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	żB	ŢB	żB	��B	��B	ĶB	żB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�?B	�EB	�B	�B	�#B	��B	�B	�B	�B	�zB	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�	B	��B	�B	�B	��B	��B	��B	�B
 B
;B
 B
 B
;B
AB
aB
GB
GB
aB
GB
B
GB
GB
[B
 �B
 iB
B
B
B
9B
%B
SB
mB
�B
YB
EB
EB
YB
tB
_B
�B
fB
�B

rB
~B
jB
jB
jB
�B
jB
jB
vB
�B
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
7B
�B
�B
�B
�B
�B
B
�B
�B
�B
 B
�B
 �B
 �B
"�B
#�B
#�B
#�B
$�B
%B
&B
%�B
%�B
%,B
'B
&2B
'B
($B
($B
($B
(XB
*KB
+B
+B
,"B
,"B
,"B
,"B
,=B
-)B
-)B
-]B
./B
./B
./B
/5B
/5B
1AB
1AB
2-B
2GB
2aB
1[B
1AB
1AB
2-B
2-B
2-B
2-B
2GB
2GB
2|B
3MB
3MB
4TB
4TB
5?B
6`B
6FB
6FB
7fB
8lB
8lB
8RB
8lB
8lB
7�B
7�B
9rB
9rB
9rB
:xB
:xB
;�B
;�B
;�B
;B
;B
;�B
<�B
<jB
<jB
<jB
<jB
<�B
<�B
<�B
<�B
<�B
<�B
>wB
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
MB
M�B
L�B
MB
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O(B
O�B
QB
Q B
Q B
Q B
QB
PB
P�B
P�B
P�B
P�B
P�B
P�B
RB
Q�B
Q�B
Q�B
R B
QB
RB
Q�B
R�B
SB
SB
R�B
R�B
SB
S&B
S&B
TFB
U2B
U2B
W$B
W
B
W
B
W
B
VB
W?B
VB
W$B
XB
XEB
X+B
XB
X+B
XB
X+B
X+B
X+B
X+B
X+B
XEB
Y1B
YKB
Y1B
Y1B
ZB
[#B
\)B
\)B
\)B
\)B
\CB
\CB
\CB
\CB
]IB
]IB
]IB
]dB
^OB
_VB
_VB
_VB
^OB
^jB
_VB
`BB
`\B
`BB
`BB
`\B
a|B
abB
bhB
bhB
bhB
bhB
cnB
cTB
dtB
cnB
cnB
dZB
dZB
c�B
c�B
dtB
ezB
ezB
e�B
ezB
ezB
f�B
f�B
ffB
g�B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
hsB
hsB
h�B
hsB
h�B
h�B
h�B
h�B
h�B
g�B
i�B
iyB
jB
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
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<r{�<7�4<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711160038222017111600382220171116003822201806221233232018062212332320180622123323201804050429172018040504291720180405042917  JA  ARFMdecpA19c                                                                20171112063507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171111213508  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171111213511  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171111213511  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171111213512  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171111213512  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171111213512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171111213512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171111213512  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171111213512                      G�O�G�O�G�O�                JA  ARUP                                                                        20171111215804                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171112153229  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20171115153822  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171115153822  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192917  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033323  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                
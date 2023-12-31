CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-05T00:35:38Z creation;2018-04-05T00:35:43Z conversion to V3.1;2019-12-19T07:41:28Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180405003538  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_227                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�X��A 1   @�X��b��@4ۘ��A�dF�#��x1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�C3D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C\C\C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�A�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�8R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��mA��mA��`A��HA��`A��`A��TA��HA���A���Aʛ�A�hsA��yAȕ�Aǝ�A�l�A�JA�t�A��A��/A�t�A�p�A�+A�VA��A�\)A�ȴA�hsA�^5A�1'A��yA���A©�A�K�A�1A��
A��!A���A��DA�~�A�ffA�K�A�&�A��jA��/A��mA�|�A�\)A�XA�Q�A�(�A���A��A��A�l�A�C�A�1A��7A��A�=qA�dZA�z�A�S�A��\A�ȴA��RA���A�-A��A���A�O�A��/A�dZA�;dA�ĜA��^A�;dA�ĜA���A�`BA�
=A�ƨA�1'A�A�S�A��-A��A�jA���A�v�A��wA��A�-A�~�A�jA��7A�E�A���A�%A��jA��
A��FA��A��-A���A�%A��+A�&�A���A�?}A�
=A��A~E�A}�PA}+A{%Azr�Ay/AuG�Ar��Aq�7Ap�`AoVAm��AmhsAlbNAk�PAkS�Ak%Aj �Ag��Ac�^Ab��Aa��A`�!A_�hA\��A[\)AZ�jAY�^AW��AV-AU�ATbAQ+AN��AMl�AK�7AJM�AIVAG�;AGdZAF~�AE�ADA�ACoABffAAdZA?��A<r�A:�RA7�TA5p�A4��A4bA3K�A2�A21A0�\A/
=A-��A+�A*��A)A'dZA%G�A#l�A!A!x�A!O�A!"�A �+A�hA�`A�wAĜAM�A�AA��A��A1'AO�A�RA$�At�A�+A�An�A�PA$�A|�A�yAQ�AA7LA��A{A�PA
=A
�+A	��A�`A��A5?A��A�AM�Ap�A��A�RA�TA
=A��AE�A��A"�A�A ĜA 1@��+@�C�@���@�M�@��@�@�Q�@�33@�@���@� �@���@�C�@�X@�ƨ@�M�@�/@���@�j@�l�@���@�@�I�@�K�@ݙ�@�9X@ڧ�@�X@ش9@���@ԋD@�(�@ӍP@��@�5?@��#@�hs@�j@��@�%@��;@˕�@ʏ\@�@���@��@��@��H@ư!@Ƨ�@Ɵ�@���@ă@î@���@��T@�X@��/@�Z@�"�@��@��7@�`B@��F@�"�@���@�@��@��F@��y@�$�@��T@��7@�/@���@� �@��@�|�@�K�@�+@�o@��!@�{@��@�`B@�V@��@��@���@�K�@�ff@�@�?}@���@���@��@�|�@�l�@�\)@�"�@�^5@��@��@��j@��D@�Z@�b@��F@��P@�+@��H@�ȴ@�~�@�@��h@�O�@��@��u@�r�@�j@�I�@�1'@��;@�33@���@��@��R@���@��T@��-@���@�7L@��/@��u@�Z@�bN@�1@���@��@�|�@�K�@�;d@�+@��y@���@���@��\@�^5@���@���@��^@��^@���@���@�ȴ@���@�V@��@���@�x�@�V@���@�1'@�b@�  @�ƨ@�o@��@�ȴ@���@�v�@�5?@��h@�O�@�?}@�7L@��@���@�z�@�bN@�Z@�I�@�A�@�1'@�1@���@�S�@�33@�+@��@�o@��H@���@��\@�ff@�E�@�5?@�J@���@���@��@�p�@�?}@�&�@���@��@�A�@�1'@��@��
@�|�@�+@��y@�n�@��@��T@�?}@��/@��@��@��u@�(�@���@�K�@�+@��@���@���@�v�@�@��^@���@�X@�/@��@���@��@�9X@�1@��@�l�@�K�@�+@�
=@��@�ȴ@��\@�ff@��@��-@���@��7@�`B@���@���@��@�A�@��@��w@��@���@�|�@�;d@��H@���@��\@�^5@�=q@�@���@�&�@�%@���@��@��u@�r�@�Q�@�(�@�1@�  @�  @�  @��;@��w@���@��@�K�@�
=@��!@�~�@�-@���@��-@�X@�G�@��@��@���@��@��u@��u@��@�j@�Q�@��@;d@~�y@~{@}/@|��@|(�@{ƨ@{��@{t�@{dZ@{@z��@z^5@zJ@y�#@y��@x�9@w�@w\)@wK�@w�@vv�@v$�@u�T@u�-@u��@u�@up�@uO�@u/@u�@uV@t��@t��@tZ@tI�@tI�@t9X@s��@sC�@s"�@so@r��@q�@q&�@q�@p��@p�9@p�u@p�@pbN@pb@o�w@oK�@n�+@nV@nE�@m�@m��@m�@mp�@m`B@m?}@m/@l�/@lZ@l�@k�
@kt�@k@jn�@j=q@j-@i�#@h��@h�9@h��@h�u@h�u@hbN@g�@g
=@fff@e�h@d�/@d��@d(�@c�
@c��@c��@cdZ@cC�@c@b��@b=q@a��@a��@ahs@a�@`��@`�u@`bN@`b@_+@^��@^$�@]�-@]?}@\�@\9X@[C�@Z~�@Z�@Y��@Y&�@XĜ@X�u@X1'@W�@W��@Wl�@W;d@W
=@V�+@V$�@U�@U�T@U��@U@Up�@T��@T�j@Tz�@T9X@S��@S�m@S�
@SdZ@So@S@R�H@R��@R��@R�@Q��@Q7L@P�@P �@P  @O�P@O+@N�y@Nȴ@Nv�@M��@M@M�-@MO�@L�@Lz�@LI�@L9X@L(�@K��@K�
@K�@K@J��@J-@I�@I��@I��@Ihs@IX@I�@H�@H �@G�@G�@G��@G�P@G�@F��@F$�@EO�@D�@D�@Dj@DZ@D9X@D1@CdZ@B�!@Bn�@B=q@B�@A��@A�#@A��@A��@A�^@A��@A��@A�7@Ax�@@Ĝ@@�@@r�@@Q�@@Q�@@1'@@ �@@b@@  @?�@?�;@?�@?��@?�P@?+@>�R@>ff@>5?@>@=�T@=�@<��@<�@<�/@<��@<�j@<z�@;�
@;��@;S�@;33@:��@:~�@:-@9�@9��@9�@8��@8�u@8  @7��@7�P@7|�@7K�@6��@6��@6�@6�@6�@6�@6�R@6��@6�+@6v�@6ff@6{@5��@5`B@5O�@5O�@5O�@4�/@4��@4j@4�@3��@3"�@3@2�!@2=q@1��@1x�@1X@1G�@1&�@1&�@1&�@1%@0�`@0bN@/�@/�;@/��@/|�@/�@.�y@.ȴ@.{@-�T@-@-�h@-`B@-�@,��@,�D@,Z@,1@+�m@+ƨ@+ƨ@+�F@+�F@+�F@+�F@+��@+��@+dZ@+C�@+33@+o@*�@*��@*^5@*J@)��@)X@)&�@(��@(�u@(Q�@(b@'��@'��@'�P@'�P@'�P@'|�@'
=@&��@%�T@%?}@%V@$��@$�@$�/@$�/@$�/@$�/@$�/@$�@$j@$9X@$�@#�m@#ƨ@#�@#t�@#"�@"��@"~�@!��@!��@!x�@!hs@!�@ r�@ A�@ A�@  �@ b@�;@�w@�@�P@��@�P@l�@��@�y@��@V@@�h@p�@O�@?}@�@V@��@��@�@�D@z�@I�@�m@�
@��@��@dZ@C�@33@"�@"�@�@��@��@��@~�@�@��@��@x�@hs@&�@��@Q�@ �@ �@�@\)@�y@�+@�+@v�@E�@{@�T@��@�h@p�@O�@��@�D@j@Z@I�@��@��@��@�@�@dZ@S�@C�@33@@�@~�@�@�@J@�#@��@�^@��@G�@�@�`@�9@A�@�@�;@�w@�@�P@\)@l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��mA��mA��`A��HA��`A��`A��TA��HA���A���Aʛ�A�hsA��yAȕ�Aǝ�A�l�A�JA�t�A��A��/A�t�A�p�A�+A�VA��A�\)A�ȴA�hsA�^5A�1'A��yA���A©�A�K�A�1A��
A��!A���A��DA�~�A�ffA�K�A�&�A��jA��/A��mA�|�A�\)A�XA�Q�A�(�A���A��A��A�l�A�C�A�1A��7A��A�=qA�dZA�z�A�S�A��\A�ȴA��RA���A�-A��A���A�O�A��/A�dZA�;dA�ĜA��^A�;dA�ĜA���A�`BA�
=A�ƨA�1'A�A�S�A��-A��A�jA���A�v�A��wA��A�-A�~�A�jA��7A�E�A���A�%A��jA��
A��FA��A��-A���A�%A��+A�&�A���A�?}A�
=A��A~E�A}�PA}+A{%Azr�Ay/AuG�Ar��Aq�7Ap�`AoVAm��AmhsAlbNAk�PAkS�Ak%Aj �Ag��Ac�^Ab��Aa��A`�!A_�hA\��A[\)AZ�jAY�^AW��AV-AU�ATbAQ+AN��AMl�AK�7AJM�AIVAG�;AGdZAF~�AE�ADA�ACoABffAAdZA?��A<r�A:�RA7�TA5p�A4��A4bA3K�A2�A21A0�\A/
=A-��A+�A*��A)A'dZA%G�A#l�A!A!x�A!O�A!"�A �+A�hA�`A�wAĜAM�A�AA��A��A1'AO�A�RA$�At�A�+A�An�A�PA$�A|�A�yAQ�AA7LA��A{A�PA
=A
�+A	��A�`A��A5?A��A�AM�Ap�A��A�RA�TA
=A��AE�A��A"�A�A ĜA 1@��+@�C�@���@�M�@��@�@�Q�@�33@�@���@� �@���@�C�@�X@�ƨ@�M�@�/@���@�j@�l�@���@�@�I�@�K�@ݙ�@�9X@ڧ�@�X@ش9@���@ԋD@�(�@ӍP@��@�5?@��#@�hs@�j@��@�%@��;@˕�@ʏ\@�@���@��@��@��H@ư!@Ƨ�@Ɵ�@���@ă@î@���@��T@�X@��/@�Z@�"�@��@��7@�`B@��F@�"�@���@�@��@��F@��y@�$�@��T@��7@�/@���@� �@��@�|�@�K�@�+@�o@��!@�{@��@�`B@�V@��@��@���@�K�@�ff@�@�?}@���@���@��@�|�@�l�@�\)@�"�@�^5@��@��@��j@��D@�Z@�b@��F@��P@�+@��H@�ȴ@�~�@�@��h@�O�@��@��u@�r�@�j@�I�@�1'@��;@�33@���@��@��R@���@��T@��-@���@�7L@��/@��u@�Z@�bN@�1@���@��@�|�@�K�@�;d@�+@��y@���@���@��\@�^5@���@���@��^@��^@���@���@�ȴ@���@�V@��@���@�x�@�V@���@�1'@�b@�  @�ƨ@�o@��@�ȴ@���@�v�@�5?@��h@�O�@�?}@�7L@��@���@�z�@�bN@�Z@�I�@�A�@�1'@�1@���@�S�@�33@�+@��@�o@��H@���@��\@�ff@�E�@�5?@�J@���@���@��@�p�@�?}@�&�@���@��@�A�@�1'@��@��
@�|�@�+@��y@�n�@��@��T@�?}@��/@��@��@��u@�(�@���@�K�@�+@��@���@���@�v�@�@��^@���@�X@�/@��@���@��@�9X@�1@��@�l�@�K�@�+@�
=@��@�ȴ@��\@�ff@��@��-@���@��7@�`B@���@���@��@�A�@��@��w@��@���@�|�@�;d@��H@���@��\@�^5@�=q@�@���@�&�@�%@���@��@��u@�r�@�Q�@�(�@�1@�  @�  @�  @��;@��w@���@��@�K�@�
=@��!@�~�@�-@���@��-@�X@�G�@��@��@���@��@��u@��u@��@�j@�Q�@��@;d@~�y@~{@}/@|��@|(�@{ƨ@{��@{t�@{dZ@{@z��@z^5@zJ@y�#@y��@x�9@w�@w\)@wK�@w�@vv�@v$�@u�T@u�-@u��@u�@up�@uO�@u/@u�@uV@t��@t��@tZ@tI�@tI�@t9X@s��@sC�@s"�@so@r��@q�@q&�@q�@p��@p�9@p�u@p�@pbN@pb@o�w@oK�@n�+@nV@nE�@m�@m��@m�@mp�@m`B@m?}@m/@l�/@lZ@l�@k�
@kt�@k@jn�@j=q@j-@i�#@h��@h�9@h��@h�u@h�u@hbN@g�@g
=@fff@e�h@d�/@d��@d(�@c�
@c��@c��@cdZ@cC�@c@b��@b=q@a��@a��@ahs@a�@`��@`�u@`bN@`b@_+@^��@^$�@]�-@]?}@\�@\9X@[C�@Z~�@Z�@Y��@Y&�@XĜ@X�u@X1'@W�@W��@Wl�@W;d@W
=@V�+@V$�@U�@U�T@U��@U@Up�@T��@T�j@Tz�@T9X@S��@S�m@S�
@SdZ@So@S@R�H@R��@R��@R�@Q��@Q7L@P�@P �@P  @O�P@O+@N�y@Nȴ@Nv�@M��@M@M�-@MO�@L�@Lz�@LI�@L9X@L(�@K��@K�
@K�@K@J��@J-@I�@I��@I��@Ihs@IX@I�@H�@H �@G�@G�@G��@G�P@G�@F��@F$�@EO�@D�@D�@Dj@DZ@D9X@D1@CdZ@B�!@Bn�@B=q@B�@A��@A�#@A��@A��@A�^@A��@A��@A�7@Ax�@@Ĝ@@�@@r�@@Q�@@Q�@@1'@@ �@@b@@  @?�@?�;@?�@?��@?�P@?+@>�R@>ff@>5?@>@=�T@=�@<��@<�@<�/@<��@<�j@<z�@;�
@;��@;S�@;33@:��@:~�@:-@9�@9��@9�@8��@8�u@8  @7��@7�P@7|�@7K�@6��@6��@6�@6�@6�@6�@6�R@6��@6�+@6v�@6ff@6{@5��@5`B@5O�@5O�@5O�@4�/@4��@4j@4�@3��@3"�@3@2�!@2=q@1��@1x�@1X@1G�@1&�@1&�@1&�@1%@0�`@0bN@/�@/�;@/��@/|�@/�@.�y@.ȴ@.{@-�T@-@-�h@-`B@-�@,��@,�D@,Z@,1@+�m@+ƨ@+ƨ@+�F@+�F@+�F@+�F@+��@+��@+dZ@+C�@+33@+o@*�@*��@*^5@*J@)��@)X@)&�@(��@(�u@(Q�@(b@'��@'��@'�P@'�P@'�P@'|�@'
=@&��@%�T@%?}@%V@$��@$�@$�/@$�/@$�/@$�/@$�/@$�@$j@$9X@$�@#�m@#ƨ@#�@#t�@#"�@"��@"~�@!��@!��@!x�@!hs@!�@ r�@ A�@ A�@  �@ b@�;@�w@�@�P@��@�P@l�@��@�y@��@V@@�h@p�@O�@?}@�@V@��@��@�@�D@z�@I�@�m@�
@��@��@dZ@C�@33@"�@"�@�@��@��@��@~�@�@��@��@x�@hs@&�@��@Q�@ �@ �@�@\)@�y@�+@�+@v�@E�@{@�T@��@�h@p�@O�@��@�D@j@Z@I�@��@��@��@�@�@dZ@S�@C�@33@@�@~�@�@�@J@�#@��@�^@��@G�@�@�`@�9@A�@�@�;@�w@�@�P@\)@l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�;B
�;B
�;B
�BB
�BB
�HB
�HB
�HB
�BB
�BB
�;B
�5B
�#B
�#B
�B
��B
��B&�B1'BE�BW
B_;BbNB{�B�hB��B��B��B��B�BĜBƨB��B�TB�TB�ZB�BB�B�B(�B,B+B(�B(�B%�B%�B%�B0!BF�BL�BS�BR�BO�BW
B_;BffBiyBffBbNB\)BXB?}BO�BP�B<jBS�BG�B<jB�B'�B7LB:^B2-B)�BuB�BDB�B�B�
B�B�NBɺB�XB��B�BǮB�'B�LB�dB�3B��B��B�+BcTB49B
��B
�B
��B
��B
��B
�`B
��B
��B
�B
_;B
m�B
w�B
t�B
\)B
R�B
T�B
5?B
�B
0!B
,B
\B
hB
  B	��B	��B	�B	��B	B	�jB	��B	��B	�RB	�^B	�B	��B	r�B	R�B	v�B	k�B	bNB	T�B	;dB	I�B	M�B	C�B	1'B	#�B	1'B	�B�B��B��B��B��B�B�B�B�B�TB�
B��B��BȴB�LB��B��B�hB�oB��B��B��B��B��B�7B~�B{�Bu�Bp�Bm�BbNB^5BaHBcTB{�B|�Bw�Bo�BgmBm�BcTBffBjBhsBcTB\)B\)Bm�BffBgmBhsBbNB^5BZBT�BXBQ�B^5B_;B]/B]/B\)B[#BVBXBVBT�BP�BP�BZBT�BS�BI�BL�BN�BO�BR�BJ�BK�BW
BP�BP�BR�BXBP�BE�B@�B49B49B>wB=qBC�BL�BQ�BP�BVBZBZBVBL�BP�BT�B\)BbNBaHBZBXBcTB^5B_;B^5BcTBbNBdZBffB]/Bm�B{�Bz�B|�B}�B�B�B}�By�Bx�B�B�PB�=B�=B�DB�oB��B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B�?B�FB�B�XB�jB�dB�jB�dBÖBɺB��B��B��B�B�
B�/B�TB�`B�fB�mB�`B�`B�B�B�B�B��B��B��B��B	B	+B	DB		7B	uB	�B	�B	�B	�B	�B	�B	�B	 �B	)�B	+B	,B	/B	33B	49B	7LB	:^B	9XB	:^B	C�B	C�B	E�B	I�B	N�B	P�B	O�B	P�B	O�B	P�B	YB	[#B	ZB	]/B	]/B	bNB	gmB	hsB	jB	l�B	q�B	u�B	u�B	y�B	�B	�B	�DB	�PB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�3B	�!B	�-B	�9B	�FB	�?B	�RB	�^B	�wB	�}B	�wB	�jB	ŢB	ǮB	ǮB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�
B	�B	�B	�)B	�/B	�5B	�5B	�/B	�5B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�mB	�mB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
+B
1B
	7B
	7B
1B
1B
	7B
	7B
DB
VB
VB
PB
JB
VB
PB
PB
VB
\B
hB
hB
bB
\B
\B
hB
hB
hB
oB
hB
bB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
!�B
 �B
�B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
!�B
"�B
#�B
#�B
"�B
!�B
#�B
$�B
$�B
$�B
"�B
#�B
'�B
'�B
&�B
'�B
'�B
&�B
%�B
%�B
$�B
%�B
'�B
(�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
'�B
&�B
'�B
(�B
'�B
'�B
'�B
)�B
)�B
(�B
%�B
+B
+B
+B
)�B
(�B
'�B
%�B
'�B
'�B
(�B
+B
+B
,B
-B
-B
-B
-B
,B
,B
,B
,B
.B
.B
.B
.B
/B
/B
.B
-B
.B
/B
/B
0!B
/B
0!B
/B
0!B
33B
2-B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
8RB
9XB
:^B
:^B
:^B
9XB
8RB
8RB
8RB
8RB
:^B
;dB
;dB
;dB
<jB
=qB
<jB
;dB
>wB
>wB
<jB
<jB
=qB
?}B
@�B
@�B
?}B
?}B
?}B
>wB
?}B
@�B
B�B
B�B
C�B
C�B
C�B
B�B
A�B
B�B
D�B
C�B
D�B
D�B
B�B
B�B
C�B
B�B
E�B
F�B
F�B
G�B
F�B
F�B
D�B
D�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
I�B
H�B
G�B
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
J�B
K�B
J�B
I�B
I�B
J�B
K�B
L�B
K�B
K�B
J�B
N�B
M�B
M�B
M�B
L�B
J�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
M�B
O�B
O�B
M�B
O�B
P�B
P�B
P�B
O�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
O�B
P�B
Q�B
Q�B
Q�B
O�B
O�B
N�B
M�B
M�B
N�B
O�B
O�B
N�B
O�B
P�B
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
R�B
VB
T�B
S�B
S�B
S�B
S�B
Q�B
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
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
YB
YB
ZB
ZB
ZB
ZB
YB
XB
YB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
]/B
[#B
[#B
ZB
[#B
]/B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
]/B
^5B
_;B
^5B
_;B
^5B
_;B
^5B
^5B
^5B
]/B
`BB
aHB
`BB
_;B
_;B
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
e`B
dZB
e`B
dZB
dZB
dZB
cTB
e`B
ffB
ffB
ffB
e`B
ffB
e`B
ffB
ffB
ffB
e`B
e`B
gmB
ffB
gmB
gmB
gmB
gmB
hsB
gmB
gmB
gmB
hsB
gmB
ffB
ffB
ffB
gmB
hsB
hsB
gmB
ffB
gmB
iyB
iyB
hsB
gmB
iyB
iyB
l�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
k�B
m�B
m�B
m�B
l�B
m�B
o�B
n�B
o�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�;B
�;B
�;B
�BB
�BB
�HB
�HB
�HB
�BB
�\B
�VB
�jB
��B
��B
ٚB
��B
��B'�B2-BF�BW�B_�Bc B|B��B��B�\B��B��B��BĶB�+BуB�nB��B�B�;BoB�B�B)B,"B+6B)_B)�B'B'�B'�B0�BF�BL�BTBS�BQ BX+B`'Bf�Bi�BgBc�B]�BZBCBRBSuB@4BVBJXB?HB�B)�B8RB;0B3MB+QB�BB�B�2B�}B�1B�"B�B̳B��B��B�$BɆB�nB��B�jB��B��B��B�	Bg�B9�BB
�UB
��B
�cB  B
��B
��B
�WB
��B
d&B
p�B
y	B
u�B
^�B
T�B
V9B
8�B
 'B
0�B
-B
�B
oB
B	�FB	�}B	�sB	�B	��B	��B	�0B	��B	�rB	��B	��B	�CB	v+B	WYB	xB	l�B	c�B	V�B	>�B	K)B	N�B	EB	3�B	%zB	1�B	B��B��B��B�B�`B�TB��B�B��B��B��BՁB�B�rB��B�dB�NB��B�2B��B��B��B��B��B�^B� B}�BxBr�Bo�Bd�BaBc�BeB|6B}<BxRBp�Bh�Bn�BeBg�BkkBiyBd�B^B]~BnIBg�BhXBiyBcnB_�B[qBV�BYeBS�B_B`B^5B^B]B[�BW
BX�BV�BU�BR BQ�BZ�BU�BT�BKBM�BO�BP�BS�BK�BL�BWsBQ�BQ�BS�BX_BQ�BF�BA�B6zB6+B@ B?.BEBM�BR�BRBV�BZ�BZ�BV�BN<BRBVB\�Bb�Ba�B[#BYKBc�B_;B`'B_pBdZBcnBe`BgRB_!Bn}B|PB{dB}qB~�B�aB��B~�B{Bz*B��B��B�B��B��B�B�$B��B��B��B�B�_B�{B�EB�7B�EB�!B�bB�TB��B��B��B��B�;B��B��B�B�<B�PB�3B�=B� B�@B�MBևB׍B�~B�B�B�B�B��B��B��B�B��B�B�2B�BB�VB��B	�B	�B	�B		�B	�B	�B	�B	�B	�B	B	#B	!B	!HB	*0B	+6B	,WB	/iB	3�B	4�B	7�B	:xB	9�B	:�B	C�B	C�B	E�B	J#B	N�B	Q B	P.B	QB	PHB	QNB	Y1B	[=B	ZQB	]dB	]�B	b�B	g�B	h�B	j�B	l�B	q�B	u�B	vB	zDB	�B	�mB	�^B	��B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�MB	��B	�|B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	żB	��B	��B	��B	�B	�B	��B	��B	�B	�B	�4B	�&B	�$B	�+B	�+B	�+B	�+B	�?B	�mB	�KB	�CB	�IB	�OB	�jB	�dB	�jB	ބB	�VB	�\B	�\B	�vB	�vB	�bB	�tB	�tB	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�*B	�$B	�B	�B	�"B	�B	�B	�(B	�<B	�VB	�.B
3B
gB
EB
KB
	RB
	lB
fB
fB
	lB
	�B
�B
pB
pB
�B
�B
pB
�B
�B
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
!�B
 �B
�B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
!�B
"�B
#�B
#�B
"�B
"B
$B
$�B
$�B
%B
# B
$&B
'�B
(
B
'B
(
B
'�B
'B
%�B
&2B
%B
&B
(
B
)B
(
B
($B
)B
(�B
)B
)*B
)B
($B
'B
($B
)B
($B
($B
($B
*B
*0B
)*B
&2B
+B
+B
+B
*B
)B
($B
&LB
(>B
(>B
)*B
+6B
+QB
,"B
-CB
-CB
-)B
-)B
,"B
,=B
,=B
,WB
./B
./B
./B
./B
/5B
/5B
.cB
-wB
.IB
/OB
/OB
0UB
/�B
0UB
/�B
0oB
3MB
2aB
4nB
4nB
5tB
5tB
5ZB
6`B
6`B
6`B
6`B
6zB
6`B
7fB
8RB
8lB
8lB
7�B
7�B
8lB
8lB
8�B
9rB
9rB
9rB
8�B
9rB
:^B
:�B
:xB
9�B
8�B
8�B
8�B
8�B
:xB
;B
;�B
;�B
<�B
=�B
<�B
;�B
>�B
>�B
<�B
<�B
=�B
?�B
@�B
@�B
?�B
?�B
?�B
>�B
?�B
@�B
B�B
B�B
C�B
C�B
C�B
B�B
A�B
B�B
D�B
C�B
D�B
D�B
B�B
B�B
C�B
B�B
E�B
F�B
F�B
G�B
F�B
F�B
D�B
D�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
I�B
H�B
G�B
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
J�B
K�B
J�B
J	B
I�B
J�B
K�B
L�B
K�B
K�B
J�B
N�B
NB
M�B
NB
L�B
J�B
M�B
M�B
M�B
NB
NB
M�B
N�B
N"B
NB
O�B
O�B
N"B
PB
Q B
Q B
QB
O�B
Q�B
RB
Q�B
R�B
R�B
RB
RB
RB
RB
RB
QB
P.B
Q B
Q�B
Q�B
Q�B
PB
PB
OB
NB
N"B
OB
O�B
O�B
OB
PB
QB
SB
R�B
SB
SB
SB
R B
RB
R B
S@B
VB
UB
T,B
TB
TB
TB
R:B
U2B
VB
V9B
V9B
VB
V9B
W$B
W?B
W?B
Y1B
Y1B
ZB
ZB
ZB
ZB
ZB
ZB
YKB
Y1B
Z7B
ZB
Z7B
Z7B
Y1B
XEB
Y1B
Z7B
Z7B
[=B
[=B
\CB
\CB
\CB
\CB
]IB
^5B
^5B
^OB
]dB
[WB
[WB
ZkB
[WB
]dB
_;B
_;B
_;B
_;B
_;B
_VB
_VB
^jB
]IB
^OB
_;B
^OB
_pB
^OB
_;B
^jB
^OB
^jB
]�B
`\B
a|B
`\B
_pB
_�B
bhB
cTB
cnB
cnB
cnB
cnB
dZB
dtB
dZB
dtB
ezB
d�B
e`B
d�B
dtB
dtB
c�B
ezB
ffB
ffB
ffB
ezB
ffB
ezB
f�B
f�B
ffB
e�B
ezB
g�B
f�B
gmB
g�B
g�B
gmB
hsB
g�B
g�B
g�B
hsB
g�B
f�B
f�B
f�B
g�B
h�B
h�B
g�B
f�B
g�B
i�B
i�B
h�B
g�B
i�B
i�B
l�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
k�B
m�B
m�B
m�B
l�B
m�B
o�B
n�B
o�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804090040092018040900400920180409004009201806221328382018062213283820180622132838201804261707082018042617070820180426170708  JA  ARFMdecpA19c                                                                20180405093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180405003538  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180405003541  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180405003541  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180405003542  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180405003542  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180405003542  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180405003542  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180405003543  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180405003543                      G�O�G�O�G�O�                JA  ARUP                                                                        20180405005633                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180405153428  CV  JULD            G�O�G�O�F�š                JM  ARCAJMQC2.0                                                                 20180408154009  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180408154009  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180426080708  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042838  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                
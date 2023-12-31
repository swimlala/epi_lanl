CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:18:40Z creation;2022-06-04T19:18:41Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191840  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               .A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�
���l1   @�
����@/M�����cz�t�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  BB�33B�33B�  C L�C� C  C  C�fC
  C�C�C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(33C)�3C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF33CG�fCI�fCK�fCM�fCP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`fD`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy�fDz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǃ3D��3D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D�|�D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@p�@w
>@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��C B�Cu�C��C��C�)C	��C]C]C�)C��C��C��C��C��C��C��C��C!��C#��C%��C((�C)��C+��C-��C/��C1�)C3��C5��C7��C9��C;��C=��C?��CA��CC��CF(�CG�)CI�)CK�)CM�)CO��CQ��CS��CU��CW��CY��C[�)C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq�)Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C��C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD`�D`��D`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy��Dy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�Dǁ�D���D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�{�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AЛ	AУ�AЫkAЪ�AЪeAЫ6AЪeAЙeAЊ	AЖA�d�A�$�A��A�bA�PA��A�	�A�+A��A�;A���A��]A���A���A��rA���A��ZA���A���A���A��rA���A���A���A��fA��2A���A��ZA���A��lA���A��fA���A��+A���A���A��ZA���A���A��lA��A��
A���A���A�бA��Aϔ{A�5�A�(�A��gA�rGA���A�1[A�g�A��A��oA�OA��A�4�A��A�M�A���A�qAA���A��RA��ZA��A�^�A�%A���A��LA���A�f�A��bA�XA|�hAv$�Ap-�Ak��Ab�xA\�rAWATAR��AM��AH��AC�RAA�A<�pA;�A:C�A9��A7�DA5�bA4[WA4A3&A2n�A1e�A/-A.OA,��A,�hA'�1A$�A#y>A"_�A!!�A m�A�nA�mA��A2�AA�A,=An/AC�A�AoA�A7A�A~A0�A�	A#:AGEA�A�.A"�A_A�A��A�&A��A|�A��A^5AS�A�MA�hAS�AA�FA2aA�6AcAJ#A�A�MA�A��AjA�A��An�Az�A��A��A\)A	A�5Ae�A%A��A��A�A�A7A
�A
��A
k�A	��A	��A	_�A��AtTA	A�AP�A��A�	A\�AoiA~�Am]A!-A�5A�A��AxA�"A��A��Av`A��A�{A+A�`A�\A��A�A�A;�A{A ��A =@��g@�A @�@���@�`�@� �@��@���@�y�@�2a@��e@��\@��@�bN@���@�ی@�/�@���@�F�@�ԕ@���@�p�@��@�kQ@�� @��@@�t�@�e,@�_p@� i@�tT@�@��X@�0U@��@�@�M@��@���@�p;@�!@�0�@�Xy@�{@��0@�P�@��B@��@�@��@�rG@��@�b@���@�$t@��@�C�@�,=@��6@�Dg@�}@�1'@�[@�S&@��H@�}@��@䛦@��@�}�@��@�j@�_@�ff@��@���@��@߬q@��?@�@�@ݚk@�Mj@�(@��[@܄�@�PH@��)@�q@�u%@�M@�-�@���@ٹ�@�B�@�PH@��Q@מ�@�p�@�:�@�q�@�`B@�YK@�~@��.@�ԕ@ӂ�@�\)@�%F@��	@� i@ҍ�@�H�@�|@�a@�X@�8�@Ϫ�@���@���@�Q�@́@�.I@̼j@��@ˌ~@���@��@ɜ@�N<@Ȳ�@�_@���@ǃ{@�hs@�;d@�;@�ȴ@�z�@�~@���@Ů@�k�@��5@�c @ò-@�8@��X@�kQ@�7@���@�&@���@�Ov@���@��@���@���@� �@��@�	@��#@���@�Vm@���@���@�)�@��@��3@�x�@��@�n�@���@�Q�@���@���@�a|@�u%@��j@�iD@�^�@�$t@���@��/@��6@�'R@���@�$t@��2@�e@�V@��x@���@�w�@��@���@��V@��"@��@�͟@�!�@�@�g�@�(@�/@�K�@��@��X@�\�@�2�@��@��@�rG@�a@�ѷ@�w�@��@��@�<6@�	l@��/@���@���@�kQ@���@�*0@�-w@��@�u%@�Q@�H@�>B@��;@�ƨ@��'@�N<@���@��[@�_�@�8@��@�j@���@�V@��@�U2@��Z@���@���@�H�@��P@���@���@�6@���@���@�G�@�!�@���@�V@��@���@���@�|�@�RT@�!�@���@�� @�q�@�$@��f@�;d@��c@���@���@��@�H�@���@�e,@��@��2@���@�4n@�@��t@�f�@�.I@���@���@�<�@�4@�J@���@��c@���@�j@���@���@�Y�@�7L@��@��'@�� @�h
@�?�@��@�p�@�!�@��@��@�h�@�1'@���@�خ@��@���@�8�@���@�ѷ@���@�K^@��@��@��W@�ݘ@��@���@�`B@�J#@�0�@�q@��@�bN@�-@��@��K@��X@���@�IR@�+@�	l@�ߤ@��e@��A@�Xy@��@��F@��@�l�@�]�@��@��@���@�j@�%�@��;@���@���@�X�@�F�@�4�@��@��v@���@�:*@��&@��@@�Y@��	@�ߤ@��f@���@���@�z�@�>B@��@��'@�E9@�0�@��!@��Y@�Xy@�0U@�
�@��
@�Y�@��@���@��@���@�l"@�
�@6z@~��@~�]@~��@~V@}��@}J�@|oi@{�W@{��@{C@zں@z�\@z:*@z_@y�>@y�H@y&�@x��@x<�@w�@wH�@w6z@w�@v��@vn�@u��@u8�@t�9@t(�@s�[@sU�@s i@r�}@rkQ@r	@q��@q��@qx�@q8�@p��@o�r@o�	@oiD@o@n��@m�@m��@l�@k�;@k~�@j��@i�D@i8�@h��@g�@go�@g�@f�8@f�y@f�@f#:@e�9@ee,@e!�@d�@d�@dG@c�0@ciD@c(@b��@bkQ@b
�@aG�@`�e@`(�@_�V@_9�@_
=@^� @^d�@]��@]Vm@]�@\�`@\�I@\~@[RT@Z�R@Zh
@Y�@Yw2@YV@X�`@X�)@X�@XN�@Wݘ@WU�@W'�@V��@V�@V��@V�r@V}V@VH�@V@U�@UN<@U%@T�$@T�@T[�@S�6@S��@SC�@R�H@Rn�@R$�@Q��@Qm]@Q<6@Qq@P�|@P�@P�@Pe�@P$@O��@O'�@N�A@N=q@Nu@M�@M�S@Mx�@MX@M \@Lѷ@L�j@L��@Lh�@K�w@Kb�@K33@J�R@Jxl@J�@I�@Is�@I%F@Hw�@H*�@G�P@G�@F��@F@�@F
�@E�@E�@D�@D!@Cخ@C��@CiD@Bں@B�@A�n@ArG@A+�@A�@@Ĝ@@]d@?�6@?C�@?&@?(@>�@>s�@>
�@=��@=�h@=j@=@<�@<h�@;�]@;�{@;"�@:��@:ff@9�#@9Q�@8�v@8�u@8'R@7�@7��@7l�@7�@6��@6�h@6�6@6��@6n�@6:*@6�@5��@5|@4�f@4�O@47�@3��@3��@3e�@3�@2�c@2J�@1��@1rG@1=�@1!�@0�@0�u@0q@04n@/�@/��@/�@/t�@/�@/C@/!-@/C@/o@/
=@.��@.�@.�\@.8�@-�#@-�@-�7@-}�@-p�@-Q�@- \@,��@,�@,oi@,A�@+خ@+��@+8@*�@*��@*3�@*!�@*@)��@)s�@)#�@(��@(C-@( �@(x@(1@'�W@'��@'�@@'\)@'9�@'"�@'Y@&�@&�\@&1�@&!�@&�@%��@%��@%�@%�@%��@%��@%�@%�@%��@%s�@%X@%�@$y>@$M@#�:@#o�@#a@#RT@#;d@"��@"�F@"z@"l�@!@!�@!hs@!�@ �v@ ��@ y>@ M@�
@��@��@�$@��@Mj@.I@�@}V@��@�H@��@/@�@�.@x@�@��@�K@�@�@s@C�@"�@�@��@��@V@�@��@�n@�=@��@�h@}�@J�@*0@�@ѷ@�@�*@�$@��@P�@6z@"�@(@�@v�@�@��@k�@G�@�v@��@��@w�@e�@>B@��@��@U�@/�@
=@�X@�@6�@e@ �@��@ϫ@�@|@IR@�@�@�$@�@�.@:�@�@�+@��@��@�$@��@a@E9@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AЛ	AУ�AЫkAЪ�AЪeAЫ6AЪeAЙeAЊ	AЖA�d�A�$�A��A�bA�PA��A�	�A�+A��A�;A���A��]A���A���A��rA���A��ZA���A���A���A��rA���A���A���A��fA��2A���A��ZA���A��lA���A��fA���A��+A���A���A��ZA���A���A��lA��A��
A���A���A�бA��Aϔ{A�5�A�(�A��gA�rGA���A�1[A�g�A��A��oA�OA��A�4�A��A�M�A���A�qAA���A��RA��ZA��A�^�A�%A���A��LA���A�f�A��bA�XA|�hAv$�Ap-�Ak��Ab�xA\�rAWATAR��AM��AH��AC�RAA�A<�pA;�A:C�A9��A7�DA5�bA4[WA4A3&A2n�A1e�A/-A.OA,��A,�hA'�1A$�A#y>A"_�A!!�A m�A�nA�mA��A2�AA�A,=An/AC�A�AoA�A7A�A~A0�A�	A#:AGEA�A�.A"�A_A�A��A�&A��A|�A��A^5AS�A�MA�hAS�AA�FA2aA�6AcAJ#A�A�MA�A��AjA�A��An�Az�A��A��A\)A	A�5Ae�A%A��A��A�A�A7A
�A
��A
k�A	��A	��A	_�A��AtTA	A�AP�A��A�	A\�AoiA~�Am]A!-A�5A�A��AxA�"A��A��Av`A��A�{A+A�`A�\A��A�A�A;�A{A ��A =@��g@�A @�@���@�`�@� �@��@���@�y�@�2a@��e@��\@��@�bN@���@�ی@�/�@���@�F�@�ԕ@���@�p�@��@�kQ@�� @��@@�t�@�e,@�_p@� i@�tT@�@��X@�0U@��@�@�M@��@���@�p;@�!@�0�@�Xy@�{@��0@�P�@��B@��@�@��@�rG@��@�b@���@�$t@��@�C�@�,=@��6@�Dg@�}@�1'@�[@�S&@��H@�}@��@䛦@��@�}�@��@�j@�_@�ff@��@���@��@߬q@��?@�@�@ݚk@�Mj@�(@��[@܄�@�PH@��)@�q@�u%@�M@�-�@���@ٹ�@�B�@�PH@��Q@מ�@�p�@�:�@�q�@�`B@�YK@�~@��.@�ԕ@ӂ�@�\)@�%F@��	@� i@ҍ�@�H�@�|@�a@�X@�8�@Ϫ�@���@���@�Q�@́@�.I@̼j@��@ˌ~@���@��@ɜ@�N<@Ȳ�@�_@���@ǃ{@�hs@�;d@�;@�ȴ@�z�@�~@���@Ů@�k�@��5@�c @ò-@�8@��X@�kQ@�7@���@�&@���@�Ov@���@��@���@���@� �@��@�	@��#@���@�Vm@���@���@�)�@��@��3@�x�@��@�n�@���@�Q�@���@���@�a|@�u%@��j@�iD@�^�@�$t@���@��/@��6@�'R@���@�$t@��2@�e@�V@��x@���@�w�@��@���@��V@��"@��@�͟@�!�@�@�g�@�(@�/@�K�@��@��X@�\�@�2�@��@��@�rG@�a@�ѷ@�w�@��@��@�<6@�	l@��/@���@���@�kQ@���@�*0@�-w@��@�u%@�Q@�H@�>B@��;@�ƨ@��'@�N<@���@��[@�_�@�8@��@�j@���@�V@��@�U2@��Z@���@���@�H�@��P@���@���@�6@���@���@�G�@�!�@���@�V@��@���@���@�|�@�RT@�!�@���@�� @�q�@�$@��f@�;d@��c@���@���@��@�H�@���@�e,@��@��2@���@�4n@�@��t@�f�@�.I@���@���@�<�@�4@�J@���@��c@���@�j@���@���@�Y�@�7L@��@��'@�� @�h
@�?�@��@�p�@�!�@��@��@�h�@�1'@���@�خ@��@���@�8�@���@�ѷ@���@�K^@��@��@��W@�ݘ@��@���@�`B@�J#@�0�@�q@��@�bN@�-@��@��K@��X@���@�IR@�+@�	l@�ߤ@��e@��A@�Xy@��@��F@��@�l�@�]�@��@��@���@�j@�%�@��;@���@���@�X�@�F�@�4�@��@��v@���@�:*@��&@��@@�Y@��	@�ߤ@��f@���@���@�z�@�>B@��@��'@�E9@�0�@��!@��Y@�Xy@�0U@�
�@��
@�Y�@��@���@��@���@�l"@�
�@6z@~��@~�]@~��@~V@}��@}J�@|oi@{�W@{��@{C@zں@z�\@z:*@z_@y�>@y�H@y&�@x��@x<�@w�@wH�@w6z@w�@v��@vn�@u��@u8�@t�9@t(�@s�[@sU�@s i@r�}@rkQ@r	@q��@q��@qx�@q8�@p��@o�r@o�	@oiD@o@n��@m�@m��@l�@k�;@k~�@j��@i�D@i8�@h��@g�@go�@g�@f�8@f�y@f�@f#:@e�9@ee,@e!�@d�@d�@dG@c�0@ciD@c(@b��@bkQ@b
�@aG�@`�e@`(�@_�V@_9�@_
=@^� @^d�@]��@]Vm@]�@\�`@\�I@\~@[RT@Z�R@Zh
@Y�@Yw2@YV@X�`@X�)@X�@XN�@Wݘ@WU�@W'�@V��@V�@V��@V�r@V}V@VH�@V@U�@UN<@U%@T�$@T�@T[�@S�6@S��@SC�@R�H@Rn�@R$�@Q��@Qm]@Q<6@Qq@P�|@P�@P�@Pe�@P$@O��@O'�@N�A@N=q@Nu@M�@M�S@Mx�@MX@M \@Lѷ@L�j@L��@Lh�@K�w@Kb�@K33@J�R@Jxl@J�@I�@Is�@I%F@Hw�@H*�@G�P@G�@F��@F@�@F
�@E�@E�@D�@D!@Cخ@C��@CiD@Bں@B�@A�n@ArG@A+�@A�@@Ĝ@@]d@?�6@?C�@?&@?(@>�@>s�@>
�@=��@=�h@=j@=@<�@<h�@;�]@;�{@;"�@:��@:ff@9�#@9Q�@8�v@8�u@8'R@7�@7��@7l�@7�@6��@6�h@6�6@6��@6n�@6:*@6�@5��@5|@4�f@4�O@47�@3��@3��@3e�@3�@2�c@2J�@1��@1rG@1=�@1!�@0�@0�u@0q@04n@/�@/��@/�@/t�@/�@/C@/!-@/C@/o@/
=@.��@.�@.�\@.8�@-�#@-�@-�7@-}�@-p�@-Q�@- \@,��@,�@,oi@,A�@+خ@+��@+8@*�@*��@*3�@*!�@*@)��@)s�@)#�@(��@(C-@( �@(x@(1@'�W@'��@'�@@'\)@'9�@'"�@'Y@&�@&�\@&1�@&!�@&�@%��@%��@%�@%�@%��@%��@%�@%�@%��@%s�@%X@%�@$y>@$M@#�:@#o�@#a@#RT@#;d@"��@"�F@"z@"l�@!@!�@!hs@!�@ �v@ ��@ y>@ M@�
@��@��@�$@��@Mj@.I@�@}V@��@�H@��@/@�@�.@x@�@��@�K@�@�@s@C�@"�@�@��@��@V@�@��@�n@�=@��@�h@}�@J�@*0@�@ѷ@�@�*@�$@��@P�@6z@"�@(@�@v�@�@��@k�@G�@�v@��@��@w�@e�@>B@��@��@U�@/�@
=@�X@�@6�@e@ �@��@ϫ@�@|@IR@�@�@�$@�@�.@:�@�@�+@��@��@�$@��@a@E9@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�>B�
B��B��B��B�
B�B��B�$B��B��B�sB�>B�$B�
B��B�>B�>B�XB�$B�XB�>B�
B�
B��B��B�B��B�$B�XB�XB�
B�$B�>B�$B��B�B�
B�XB�XB�>B�$B�$B�$B�B�XB�yB�B�B�B�B�RB�B�B�@B޸B��BeFBs�B��B	�xB	�B
�hB
�qB�B�B�BEB0;B9rB
�B
� B
��B
y�B
p;B
e`B
UgB
U�B
m)B
]�B
GEB
)DB
�B	�NB	�B	�(B	wLB	^5B	1[B	!�B	�B	
	B	�B��B��B��B�}B�]B	B	 �B	 �B	�B	MB	HB	�B	B	B	4B	�B��B�B	 4B�ZBخBևB�{B�oB�B҉B��B֡BބB�qB�B	  B	�B	�B	�B	$�B	3hB	?.B	GzB	PHB	d@B	o�B	vzB	u%B	��B	��B	�B	�)B	�#B	�^B	��B	�BB	�B	�
B	�B	�!B	�<B	��B	��B	��B	�{B	�?B	�lB	ˬB	��B	�pB	�PB	��B	̘B	��B	�	B	ƨB	��B	ðB	ȀB	�XB	ɠB	�RB	�B	бB	��B	�:B	֡B	ٴB	�B	�B	�B	�B	ݲB	�/B	�jB	�;B	�B	�VB	�VB	��B	��B	�B	�2B	�B	�nB	�zB	�B	��B	��B	�PB	��B	��B	��B	��B	�.B	�B	��B	�<B	�B
 B
YB
	�B
�B
�B
�B
+B
B
	�B
KB
	B
	lB
xB
�B
�B
DB
B
^B

rB

#B

rB
B

�B
�B
�B
B
�B
�B
�B
B
�B
�B
aB
-B
-B
-B
-B
{B
�B
{B
[B
UB
 B
 �B
 B
 �B
 �B
 �B
 iB	��B	�.B	��B	�.B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�qB	�<B	�B	�jB	��B	�PB	��B	�B	��B	�B	��B	��B	��B	�B	�B	�dB	�B	��B	��B	��B	�JB	�dB	�dB	��B	��B	��B	�JB	�B	�jB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�"B	��B	�B	�<B	�B	�B	�nB	�B	�B	�B	�B	��B	�B	�LB	��B	�XB	��B	��B	�rB	�rB	��B	�B	��B	�$B	��B	��B	��B	�rB	�XB	�>B	�DB	��B	��B	��B	�xB	��B	�JB	�0B	�0B	�B	��B	��B	��B	�B	�B	�B	��B	��B	�PB	��B	��B	��B	��B	��B	�B	�VB	�"B	��B	��B	�wB	��B
  B	��B
 �B
�B
�B
�B
UB
 �B
 �B
 �B
 �B
 �B
 �B	��B	�wB	��B	��B	��B	�(B	��B
UB
�B
B
�B
�B
�B
�B
B
�B
gB
�B
�B
oB	��B	�<B	�<B	�"B	��B	�B	��B
�B
 �B	��B
  B
  B
 iB
 �B
�B
YB
�B
�B
B
�B
�B
�B
1B
�B
	7B
	lB
	�B
fB
B
�B
�B
�B
	B
	�B
�B
zB
�B
	�B
�B
	�B

�B
^B
JB
jB
�B
�B
B
B
�B
~B
�B
�B
�B
B

	B

=B
�B
jB
�B
<B
�B
6B
~B
�B
�B
�B
"B
pB
B
�B
VB
�B
B
BB
vB
BB
vB
BB
�B
�B
�B
pB
�B
�B
�B
�B
�B
vB
}B
�B
vB
�B
�B
 B
hB
�B
�B
�B
B
�B
@B
�B
�B
�B
�B
�B
�B
FB
{B
{B
�B
�B
�B
B
9B
�B

B
sB
�B
�B
+B
_B
�B
�B
�B
KB
�B
B
B
�B
	B
=B
=B
WB
WB
�B
�B
)B
CB
)B
B
�B
B
/B
dB
�B
5B
OB
�B
�B
�B
�B
VB
�B
 \B
 \B
 �B
 vB
 \B
 vB
!B
!HB
!bB
!bB
!�B
"hB
"�B
#TB
$tB
$@B
$tB
$�B
%�B
%�B
&B
%zB
%zB
&2B
&B
&fB
'mB
(XB
(XB
(sB
(
B
'�B
(XB
(sB
(sB
)�B
)B
)*B
)�B
)�B
*B
*�B
*0B
+�B
,B
,�B
,�B
-�B
.IB
.�B
/B
.�B
.IB
.IB
.IB
.IB
-wB
.cB
.�B
.�B
/�B
1B
1[B
1vB
1vB
2GB
2|B
3B
3hB
3�B
3�B
3�B
49B
4�B
5�B
5�B
6`B
6�B
7�B
7�B
7�B
8RB
8�B
8�B
8�B
8�B
9>B
9XB
9�B
:xB
:�B
:�B
:�B
:�B
;JB
;JB
;�B
<PB
<jB
<�B
=�B
=�B
>BB
>�B
>�B
?HB
?HB
?.B
?�B
?�B
?�B
@B
@OB
@iB
@�B
@�B
@�B
@�B
A�B
AoB
A�B
A�B
BuB
B�B
CB
CaB
C-B
C{B
DB
C�B
D3B
D�B
D�B
D�B
D�B
ESB
E�B
EmB
E�B
FB
FYB
F�B
F�B
F�B
F�B
F�B
G+B
G�B
G�B
HB
G�B
H1B
H1B
HB
HKB
HfB
H�B
H�B
H�B
IB
IB
IRB
I�B
J	B
J=B
JXB
J�B
J�B
KDB
K^B
KxB
K�B
K�B
L0B
L0B
L0B
L0B
L0B
L�B
MPB
M�B
M�B
M�B
M�B
M�B
N"B
N"B
N�B
NpB
N�B
N�B
OBB
O\B
O\B
O�B
O�B
PHB
PHB
P�B
P�B
Q4B
Q4B
Q�B
R B
RoB
R�B
R�B
R�B
R�B
S[B
TB
T,B
T,B
TFB
T�B
UgB
U�B
U�B
VB
U�B
V9B
VmB
V�B
WYB
WYB
WYB
W?B
W�B
XyB
X�B
Y1B
Y1B
YeB
YB
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
[	B
[WB
[qB
[�B
\B
\B
\]B
\xB
\�B
]B
\�B
\�B
]B
]/B
]dB
]~B
]�B
]�B
^5B
^5B
^�B
_B
_!B
_VB
_!B
^�B
_�B
_pB
_�B
`\B
`\B
`�B
`�B
`�B
`vB
`�B
`vB
`vB
`�B
a-B
abB
abB
a�B
a�B
a�B
a�B
bNB
bNB
b�B
b�B
b�B
cB
cB
cB
c B
c B
c�B
c�B
c�B
c�B
d&B
d@B
d�B
d�B
eFB
e�B
e�B
fB
ffB
fLB
f�B
gB
g8B
gRB
gRB
g8B
gRB
g�B
g�B
g�B
g�B
g�B
h
B
h
B
hsB
h�B
h�B
h�B
h�B
iB
i*B
i_B
iDB
i_B
iDB
iDB
iyB
iyB
i�B
i�B
jKB
jB
kB
kB
kB
k6B
k6B
kkB
k�B
k�B
k�B
lB
l"B
l"B
l�B
l�B
mB
m)B
m�B
nB
nIB
nIB
nIB
nIB
n�B
n�B
n}B
oOB
o�B
o�B
o�B
pUB
p�B
p�B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
rB
r�B
r�B
r�B
sB
sB
sB
s3B
sB
shB
shB
sMB
shB
t9B
t�B
t�B
t�B
uB
uB
t�B
uB
t�B
utB
v`B
v+B
v`B
vzB
wB
v�B
w2B
w2B
wLB
wLB
w�B
w�B
x8B
x8B
xlB
x�B
x�B
x�B
y	B
y	B
y	B
yXB
yXB
yrB
y�B
y�B
z*B
z*B
zDB
zDB
z�B
z�B
z�B
z�B
{0B
{0B
{JB
{B
{�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�>B�
B��B��B��B�
B�B��B�$B��B��B�sB�>B�$B�
B��B�>B�>B�XB�$B�XB�>B�
B�
B��B��B�B��B�$B�XB�XB�
B�$B�>B�$B��B�B�
B�XB�XB�>B�$B�$B�$B�B�XB�yB�B�B�B�B�RB�B�B�@B޸B��BeFBs�B��B	�xB	�B
�hB
�qB�B�B�BEB0;B9rB
�B
� B
��B
y�B
p;B
e`B
UgB
U�B
m)B
]�B
GEB
)DB
�B	�NB	�B	�(B	wLB	^5B	1[B	!�B	�B	
	B	�B��B��B��B�}B�]B	B	 �B	 �B	�B	MB	HB	�B	B	B	4B	�B��B�B	 4B�ZBخBևB�{B�oB�B҉B��B֡BބB�qB�B	  B	�B	�B	�B	$�B	3hB	?.B	GzB	PHB	d@B	o�B	vzB	u%B	��B	��B	�B	�)B	�#B	�^B	��B	�BB	�B	�
B	�B	�!B	�<B	��B	��B	��B	�{B	�?B	�lB	ˬB	��B	�pB	�PB	��B	̘B	��B	�	B	ƨB	��B	ðB	ȀB	�XB	ɠB	�RB	�B	бB	��B	�:B	֡B	ٴB	�B	�B	�B	�B	ݲB	�/B	�jB	�;B	�B	�VB	�VB	��B	��B	�B	�2B	�B	�nB	�zB	�B	��B	��B	�PB	��B	��B	��B	��B	�.B	�B	��B	�<B	�B
 B
YB
	�B
�B
�B
�B
+B
B
	�B
KB
	B
	lB
xB
�B
�B
DB
B
^B

rB

#B

rB
B

�B
�B
�B
B
�B
�B
�B
B
�B
�B
aB
-B
-B
-B
-B
{B
�B
{B
[B
UB
 B
 �B
 B
 �B
 �B
 �B
 iB	��B	�.B	��B	�.B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�qB	�<B	�B	�jB	��B	�PB	��B	�B	��B	�B	��B	��B	��B	�B	�B	�dB	�B	��B	��B	��B	�JB	�dB	�dB	��B	��B	��B	�JB	�B	�jB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�"B	��B	�B	�<B	�B	�B	�nB	�B	�B	�B	�B	��B	�B	�LB	��B	�XB	��B	��B	�rB	�rB	��B	�B	��B	�$B	��B	��B	��B	�rB	�XB	�>B	�DB	��B	��B	��B	�xB	��B	�JB	�0B	�0B	�B	��B	��B	��B	�B	�B	�B	��B	��B	�PB	��B	��B	��B	��B	��B	�B	�VB	�"B	��B	��B	�wB	��B
  B	��B
 �B
�B
�B
�B
UB
 �B
 �B
 �B
 �B
 �B
 �B	��B	�wB	��B	��B	��B	�(B	��B
UB
�B
B
�B
�B
�B
�B
B
�B
gB
�B
�B
oB	��B	�<B	�<B	�"B	��B	�B	��B
�B
 �B	��B
  B
  B
 iB
 �B
�B
YB
�B
�B
B
�B
�B
�B
1B
�B
	7B
	lB
	�B
fB
B
�B
�B
�B
	B
	�B
�B
zB
�B
	�B
�B
	�B

�B
^B
JB
jB
�B
�B
B
B
�B
~B
�B
�B
�B
B

	B

=B
�B
jB
�B
<B
�B
6B
~B
�B
�B
�B
"B
pB
B
�B
VB
�B
B
BB
vB
BB
vB
BB
�B
�B
�B
pB
�B
�B
�B
�B
�B
vB
}B
�B
vB
�B
�B
 B
hB
�B
�B
�B
B
�B
@B
�B
�B
�B
�B
�B
�B
FB
{B
{B
�B
�B
�B
B
9B
�B

B
sB
�B
�B
+B
_B
�B
�B
�B
KB
�B
B
B
�B
	B
=B
=B
WB
WB
�B
�B
)B
CB
)B
B
�B
B
/B
dB
�B
5B
OB
�B
�B
�B
�B
VB
�B
 \B
 \B
 �B
 vB
 \B
 vB
!B
!HB
!bB
!bB
!�B
"hB
"�B
#TB
$tB
$@B
$tB
$�B
%�B
%�B
&B
%zB
%zB
&2B
&B
&fB
'mB
(XB
(XB
(sB
(
B
'�B
(XB
(sB
(sB
)�B
)B
)*B
)�B
)�B
*B
*�B
*0B
+�B
,B
,�B
,�B
-�B
.IB
.�B
/B
.�B
.IB
.IB
.IB
.IB
-wB
.cB
.�B
.�B
/�B
1B
1[B
1vB
1vB
2GB
2|B
3B
3hB
3�B
3�B
3�B
49B
4�B
5�B
5�B
6`B
6�B
7�B
7�B
7�B
8RB
8�B
8�B
8�B
8�B
9>B
9XB
9�B
:xB
:�B
:�B
:�B
:�B
;JB
;JB
;�B
<PB
<jB
<�B
=�B
=�B
>BB
>�B
>�B
?HB
?HB
?.B
?�B
?�B
?�B
@B
@OB
@iB
@�B
@�B
@�B
@�B
A�B
AoB
A�B
A�B
BuB
B�B
CB
CaB
C-B
C{B
DB
C�B
D3B
D�B
D�B
D�B
D�B
ESB
E�B
EmB
E�B
FB
FYB
F�B
F�B
F�B
F�B
F�B
G+B
G�B
G�B
HB
G�B
H1B
H1B
HB
HKB
HfB
H�B
H�B
H�B
IB
IB
IRB
I�B
J	B
J=B
JXB
J�B
J�B
KDB
K^B
KxB
K�B
K�B
L0B
L0B
L0B
L0B
L0B
L�B
MPB
M�B
M�B
M�B
M�B
M�B
N"B
N"B
N�B
NpB
N�B
N�B
OBB
O\B
O\B
O�B
O�B
PHB
PHB
P�B
P�B
Q4B
Q4B
Q�B
R B
RoB
R�B
R�B
R�B
R�B
S[B
TB
T,B
T,B
TFB
T�B
UgB
U�B
U�B
VB
U�B
V9B
VmB
V�B
WYB
WYB
WYB
W?B
W�B
XyB
X�B
Y1B
Y1B
YeB
YB
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
[	B
[WB
[qB
[�B
\B
\B
\]B
\xB
\�B
]B
\�B
\�B
]B
]/B
]dB
]~B
]�B
]�B
^5B
^5B
^�B
_B
_!B
_VB
_!B
^�B
_�B
_pB
_�B
`\B
`\B
`�B
`�B
`�B
`vB
`�B
`vB
`vB
`�B
a-B
abB
abB
a�B
a�B
a�B
a�B
bNB
bNB
b�B
b�B
b�B
cB
cB
cB
c B
c B
c�B
c�B
c�B
c�B
d&B
d@B
d�B
d�B
eFB
e�B
e�B
fB
ffB
fLB
f�B
gB
g8B
gRB
gRB
g8B
gRB
g�B
g�B
g�B
g�B
g�B
h
B
h
B
hsB
h�B
h�B
h�B
h�B
iB
i*B
i_B
iDB
i_B
iDB
iDB
iyB
iyB
i�B
i�B
jKB
jB
kB
kB
kB
k6B
k6B
kkB
k�B
k�B
k�B
lB
l"B
l"B
l�B
l�B
mB
m)B
m�B
nB
nIB
nIB
nIB
nIB
n�B
n�B
n}B
oOB
o�B
o�B
o�B
pUB
p�B
p�B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
rB
r�B
r�B
r�B
sB
sB
sB
s3B
sB
shB
shB
sMB
shB
t9B
t�B
t�B
t�B
uB
uB
t�B
uB
t�B
utB
v`B
v+B
v`B
vzB
wB
v�B
w2B
w2B
wLB
wLB
w�B
w�B
x8B
x8B
xlB
x�B
x�B
x�B
y	B
y	B
y	B
yXB
yXB
yrB
y�B
y�B
z*B
z*B
zDB
zDB
z�B
z�B
z�B
z�B
{0B
{0B
{JB
{B
{�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105236  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191840  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191841  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191841                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041848  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041848  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                
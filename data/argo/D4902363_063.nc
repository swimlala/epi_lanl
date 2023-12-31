CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-02T00:35:21Z creation;2016-12-02T00:35:23Z conversion to V3.1;2019-12-19T08:24:09Z update;     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20161202003521  20200115111517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ?A   JA  I2_0576_063                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��tA
��1   @��t��J @:�c�A \�d���[1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�fD}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @7
=@}p�@��R@��RA\)A?\)A_\)A\)A��HA��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9��D9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|��D|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�{�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�(R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A��
A��A��A��#A��/A��;A��;A��HA��HA��;A��/A��`A��mA��yA��yA��yA��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A��A���Aɇ+A�M�A�-A�bNA��A�jA��A�JA���A�VA�t�A��-A��
A�VA�ĜA��TA��
A��mA��A��uA�"�A���A�\)A�ȴA���A��A�-A�E�A�XA���A��A��A��FA�;dA��7A�VA��HA�~�A��A�|�A�?}A���A���A���A�`BA��-A�-A��uA�-A�|�A�K�A���A���A�x�A���A�$�A��9A�^5A��A���A�/A��A��-A���A�I�A�jA��7A���A�/A��A�33A�|�A�r�A�l�A���A�5?A���A��A���A�bNA~n�A|9XAyp�Ay
=Ax�jAxQ�Aw�
AwO�AvffAuVAsƨArZApVAo��AmAlI�Aj��Ai?}Ag��AgVAfv�Ae�Ac��Ab�DAa�A`JA_7LA^9XA]�hA\I�AZ��AZA�AY��AY%AX�AW�AU��AT��ARffAO�mAO+ANjAMhsAL1'AK%AG�AF�AEAC��AC��AB��AB1'AAXA@ffA?��A<��A;`BA;VA:z�A:�A9�A85?A6�HA5�A5;dA4��A3�#A2��A2�jA2^5A0�A0r�A/t�A.�\A,��A,�\A,jA,ZA+t�A)�mA(��A&�A%G�A#�hA!?}A!+A!+A!&�A ��A VA�A��A��A;dA�-A5?AoA��A33A��AbNA��A?}A�\A  A;dA�A�`A~�AE�A�
AS�A&�AVA�yA�A��A��A��A�A/A�`A�9A$�A�7A��A1'A(�A�wA&�A�RA=qA�A�7A
~�A��AM�A��A�yA�PAr�A��A�`AM�A��A j@�|�@�Ĝ@�r�@� �@��@�ƨ@�\)@��@�ff@�7L@�@�v�@�Z@�ȴ@�~�@��@�~�@�@�5?@�Z@�F@�+@���@� �@ߕ�@��@��@���@ڸR@�X@�S�@ָR@�n�@��@��H@�=q@�/@�(�@���@ͺ^@˥�@��@�J@�`B@�bN@Ǿw@ǅ@�33@�n�@�O�@���@�t�@�Ĝ@��#@��@�9X@�C�@�-@�O�@�Q�@�K�@�J@���@��@�G�@��@�9X@���@��@�33@��!@�7L@�I�@��y@�M�@��^@�@���@��@���@�{@�%@�Ĝ@��9@��@�(�@��@�1@���@�l�@�;d@��@�=q@�/@�(�@���@���@�O�@��/@���@�Ĝ@��D@�b@��H@�n�@���@��@�/@��@�V@�z�@���@�
=@��\@�{@�p�@���@��@�ƨ@�t�@��@���@�~�@��@��@�Z@�I�@�9X@��@��m@��@�33@��\@��\@��\@��+@��T@�7L@���@� �@�C�@�o@��@�@��H@���@�E�@��T@��h@�G�@�&�@��/@�(�@�ƨ@�dZ@�K�@�"�@���@�ȴ@��\@�^5@�{@��@��T@��T@�J@�@��@��`@���@�Ĝ@��j@��@�9X@��m@��w@���@�|�@�C�@�o@�ȴ@�~�@��^@�O�@�/@�V@���@�Ĝ@��9@�j@�bN@�  @�;@�@
=@~ȴ@~ȴ@�@�;@�z�@�b@�w@�@��@|�@~�y@~{@}p�@}��@}�-@}��@}��@}�@}�@}/@|�j@|��@|��@|I�@{��@y�@x��@y�@zM�@y�^@y&�@x��@x1'@x �@x �@x  @w�w@vȴ@vV@vE�@v{@u�@t��@tz�@s��@s�
@s�F@s��@st�@r��@r�!@r�\@rM�@q�@q��@qhs@qX@qG�@q&�@p��@p��@p�@o��@o;d@o�@n�@n�R@n��@n@l��@l(�@k�
@k��@k��@l(�@kdZ@jn�@jn�@j^5@jM�@jJ@h��@h �@h �@g�;@g�P@g;d@g\)@g
=@f�+@f@e��@e�-@e�h@eO�@d�@d��@d�j@dI�@c�@cS�@cC�@cC�@c��@cƨ@b�H@bn�@b-@b�@a�@aX@a%@`Q�@_l�@^��@^ȴ@^��@^v�@^E�@]�@]p�@\�D@\1@[ƨ@[��@[t�@[@Z��@Zn�@ZM�@Z�@Y��@Y��@Y7L@X��@X��@X�@X  @W�;@W�P@W+@V��@V��@V5?@U�T@U`B@T�j@Tz�@TZ@T9X@T(�@S��@Sƨ@S�F@St�@SC�@S"�@S@R��@R=q@Q�@Q�#@Q��@Qx�@Q�@P�9@PbN@PA�@P1'@O�@O��@O��@Ol�@OK�@N�@Nff@N{@M��@M�@L��@L��@L�/@L�j@L�D@Lz�@LZ@L�@K�@KC�@K33@K@J�H@J�\@J~�@J�@I�#@I7L@H��@H1'@G�@G��@G�w@G\)@F�R@FE�@E�-@EO�@D�j@D�D@Dz�@DZ@DZ@DZ@D�@C�m@CC�@B��@B�\@B~�@B~�@Bn�@BJ@A��@Ax�@Ahs@AX@AG�@@��@@��@@bN@@Q�@@1'@@ �@?�P@?
=@>ff@>$�@=�@=��@=�-@=O�@=�@<�D@<�@;ƨ@;��@;�@;dZ@;S�@;C�@;"�@;@:�@:��@:�!@:�\@:~�@:M�@:�@9�@9G�@8�u@8A�@7�;@7�@7�P@7\)@7+@6�@6�R@6��@6ff@6E�@6{@5�-@5/@4�j@4�D@4z�@4Z@4(�@3��@3�
@3�F@3��@3�@3t�@3t�@3t�@3dZ@3dZ@3dZ@3S�@333@3"�@2��@2-@1��@1�@1��@1��@1��@1hs@0�`@0r�@01'@/�;@/�w@/�@/�@/��@/��@/��@/��@/�P@/�P@/�P@/|�@/K�@/�@.��@.�y@.�@.��@.��@.ff@.5?@-�T@-@-�h@-�@-V@,��@,��@,��@,�D@,z�@,z�@,j@,1@+�m@+��@+C�@*~�@)��@)�7@)&�@(�9@(�@(Q�@'�@'�;@'��@'�w@'��@'|�@'l�@'K�@'K�@'K�@&�y@&�R@&�+@%�-@%�@$�j@$(�@#�
@#dZ@"�@"n�@!�7@!&�@ ��@ �`@ �9@ �u@ r�@ bN@ Q�@�@�w@|�@�@�R@E�@�-@��@�@?}@�@��@�j@�@�D@j@9X@�m@�
@��@t�@@�H@�\@^5@-@��@�@�#@�^@x�@hs@G�@7L@&�@�`@�9@�@A�@ �@�@�w@��@��@|�@|�@\)@\)@K�@K�@+@�@
=@�y@ȴ@��@v�@ff@E�@$�@@@@��@`B@/@�@V@�/@�/@�j@�@��@�D@j@Z@(�@�m@�@"�@��@��@��@��@M�@=q@-@J@�#@�7@��@�7@x�@X@�`@�u@Q�@A�@  @�@�@�R@�+@ff@@��@@��@�@��@�@��@z�@9X@1@��@��@�m@��@33@o@
�!@
^5@
-@	�@	��@	x�@	&�@��@�@A�@b@�;@�w@��@�P@�P@l�@K�@�@
=@
=@�y@�R@5?@{@@�@�@�@�T@��@@�-@p�@?}@V@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A��
A��A��A��#A��/A��;A��;A��HA��HA��;A��/A��`A��mA��yA��yA��yA��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A��A���Aɇ+A�M�A�-A�bNA��A�jA��A�JA���A�VA�t�A��-A��
A�VA�ĜA��TA��
A��mA��A��uA�"�A���A�\)A�ȴA���A��A�-A�E�A�XA���A��A��A��FA�;dA��7A�VA��HA�~�A��A�|�A�?}A���A���A���A�`BA��-A�-A��uA�-A�|�A�K�A���A���A�x�A���A�$�A��9A�^5A��A���A�/A��A��-A���A�I�A�jA��7A���A�/A��A�33A�|�A�r�A�l�A���A�5?A���A��A���A�bNA~n�A|9XAyp�Ay
=Ax�jAxQ�Aw�
AwO�AvffAuVAsƨArZApVAo��AmAlI�Aj��Ai?}Ag��AgVAfv�Ae�Ac��Ab�DAa�A`JA_7LA^9XA]�hA\I�AZ��AZA�AY��AY%AX�AW�AU��AT��ARffAO�mAO+ANjAMhsAL1'AK%AG�AF�AEAC��AC��AB��AB1'AAXA@ffA?��A<��A;`BA;VA:z�A:�A9�A85?A6�HA5�A5;dA4��A3�#A2��A2�jA2^5A0�A0r�A/t�A.�\A,��A,�\A,jA,ZA+t�A)�mA(��A&�A%G�A#�hA!?}A!+A!+A!&�A ��A VA�A��A��A;dA�-A5?AoA��A33A��AbNA��A?}A�\A  A;dA�A�`A~�AE�A�
AS�A&�AVA�yA�A��A��A��A�A/A�`A�9A$�A�7A��A1'A(�A�wA&�A�RA=qA�A�7A
~�A��AM�A��A�yA�PAr�A��A�`AM�A��A j@�|�@�Ĝ@�r�@� �@��@�ƨ@�\)@��@�ff@�7L@�@�v�@�Z@�ȴ@�~�@��@�~�@�@�5?@�Z@�F@�+@���@� �@ߕ�@��@��@���@ڸR@�X@�S�@ָR@�n�@��@��H@�=q@�/@�(�@���@ͺ^@˥�@��@�J@�`B@�bN@Ǿw@ǅ@�33@�n�@�O�@���@�t�@�Ĝ@��#@��@�9X@�C�@�-@�O�@�Q�@�K�@�J@���@��@�G�@��@�9X@���@��@�33@��!@�7L@�I�@��y@�M�@��^@�@���@��@���@�{@�%@�Ĝ@��9@��@�(�@��@�1@���@�l�@�;d@��@�=q@�/@�(�@���@���@�O�@��/@���@�Ĝ@��D@�b@��H@�n�@���@��@�/@��@�V@�z�@���@�
=@��\@�{@�p�@���@��@�ƨ@�t�@��@���@�~�@��@��@�Z@�I�@�9X@��@��m@��@�33@��\@��\@��\@��+@��T@�7L@���@� �@�C�@�o@��@�@��H@���@�E�@��T@��h@�G�@�&�@��/@�(�@�ƨ@�dZ@�K�@�"�@���@�ȴ@��\@�^5@�{@��@��T@��T@�J@�@��@��`@���@�Ĝ@��j@��@�9X@��m@��w@���@�|�@�C�@�o@�ȴ@�~�@��^@�O�@�/@�V@���@�Ĝ@��9@�j@�bN@�  @�;@�@
=@~ȴ@~ȴ@�@�;@�z�@�b@�w@�@��@|�@~�y@~{@}p�@}��@}�-@}��@}��@}�@}�@}/@|�j@|��@|��@|I�@{��@y�@x��@y�@zM�@y�^@y&�@x��@x1'@x �@x �@x  @w�w@vȴ@vV@vE�@v{@u�@t��@tz�@s��@s�
@s�F@s��@st�@r��@r�!@r�\@rM�@q�@q��@qhs@qX@qG�@q&�@p��@p��@p�@o��@o;d@o�@n�@n�R@n��@n@l��@l(�@k�
@k��@k��@l(�@kdZ@jn�@jn�@j^5@jM�@jJ@h��@h �@h �@g�;@g�P@g;d@g\)@g
=@f�+@f@e��@e�-@e�h@eO�@d�@d��@d�j@dI�@c�@cS�@cC�@cC�@c��@cƨ@b�H@bn�@b-@b�@a�@aX@a%@`Q�@_l�@^��@^ȴ@^��@^v�@^E�@]�@]p�@\�D@\1@[ƨ@[��@[t�@[@Z��@Zn�@ZM�@Z�@Y��@Y��@Y7L@X��@X��@X�@X  @W�;@W�P@W+@V��@V��@V5?@U�T@U`B@T�j@Tz�@TZ@T9X@T(�@S��@Sƨ@S�F@St�@SC�@S"�@S@R��@R=q@Q�@Q�#@Q��@Qx�@Q�@P�9@PbN@PA�@P1'@O�@O��@O��@Ol�@OK�@N�@Nff@N{@M��@M�@L��@L��@L�/@L�j@L�D@Lz�@LZ@L�@K�@KC�@K33@K@J�H@J�\@J~�@J�@I�#@I7L@H��@H1'@G�@G��@G�w@G\)@F�R@FE�@E�-@EO�@D�j@D�D@Dz�@DZ@DZ@DZ@D�@C�m@CC�@B��@B�\@B~�@B~�@Bn�@BJ@A��@Ax�@Ahs@AX@AG�@@��@@��@@bN@@Q�@@1'@@ �@?�P@?
=@>ff@>$�@=�@=��@=�-@=O�@=�@<�D@<�@;ƨ@;��@;�@;dZ@;S�@;C�@;"�@;@:�@:��@:�!@:�\@:~�@:M�@:�@9�@9G�@8�u@8A�@7�;@7�@7�P@7\)@7+@6�@6�R@6��@6ff@6E�@6{@5�-@5/@4�j@4�D@4z�@4Z@4(�@3��@3�
@3�F@3��@3�@3t�@3t�@3t�@3dZ@3dZ@3dZ@3S�@333@3"�@2��@2-@1��@1�@1��@1��@1��@1hs@0�`@0r�@01'@/�;@/�w@/�@/�@/��@/��@/��@/��@/�P@/�P@/�P@/|�@/K�@/�@.��@.�y@.�@.��@.��@.ff@.5?@-�T@-@-�h@-�@-V@,��@,��@,��@,�D@,z�@,z�@,j@,1@+�m@+��@+C�@*~�@)��@)�7@)&�@(�9@(�@(Q�@'�@'�;@'��@'�w@'��@'|�@'l�@'K�@'K�@'K�@&�y@&�R@&�+@%�-@%�@$�j@$(�@#�
@#dZ@"�@"n�@!�7@!&�@ ��@ �`@ �9@ �u@ r�@ bN@ Q�@�@�w@|�@�@�R@E�@�-@��@�@?}@�@��@�j@�@�D@j@9X@�m@�
@��@t�@@�H@�\@^5@-@��@�@�#@�^@x�@hs@G�@7L@&�@�`@�9@�@A�@ �@�@�w@��@��@|�@|�@\)@\)@K�@K�@+@�@
=@�y@ȴ@��@v�@ff@E�@$�@@@@��@`B@/@�@V@�/@�/@�j@�@��@�D@j@Z@(�@�m@�@"�@��@��@��@��@M�@=q@-@J@�#@�7@��@�7@x�@X@�`@�u@Q�@A�@  @�@�@�R@�+@ff@@��@@��@�@��@�@��@z�@9X@1@��@��@�m@��@33@o@
�!@
^5@
-@	�@	��@	x�@	&�@��@�@A�@b@�;@�w@��@�P@�P@l�@K�@�@
=@
=@�y@�R@5?@{@@�@�@�@�T@��@@�-@p�@?}@V@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B��B��B��B�}B�}B��B��BBB��B��B��BBBBBÖBBBÖBÖBÖBÖBĜBŢBƨBȴB��B��B��B�
B�B�)B�NB�TB�mB�fB�fB�yB�B�sB�fB�B��B��B��BƨBÖB��B�}B�?B�!B�!B��B��B��B��B�oB�DB�%B�B�B}�Bo�BbNB\)BYBN�BH�BD�B9XB)�B�B	7BB  B��B�B�B�B�B�HB��B��B�?B�3B�-B��B�Bu�Bk�B[#BK�B@�B5?B'�B�B�B\B1B
��B
�B
�B
ȴB
ĜB
�qB
�RB
�B
��B
�7B
� B
k�B
iyB
gmB
e`B
bNB
_;B
YB
P�B
C�B
;dB
-B
$�B
�B
bB
	7B	��B	�B	�B	�fB	�;B	��B	ƨB	�wB	�LB	�'B	��B	��B	��B	�bB	�=B	�1B	�B	{�B	r�B	jB	^5B	T�B	D�B	?}B	9XB	0!B	&�B	�B		7B��B�B�yB�fB�NB�/B�B��B��B�TB�)B�#B�BB�;B�5B�#B�
B��B��B��BɺBŢBB��B�XB�?B�!B�!B�B��B��B��B�B��B��B��B��B�uB�PB�JB�JB�JB�JB�DB�7B�1B�+B�%B�B� B|�Bx�Bv�Bu�Bt�Bq�Bp�Bn�Bl�BjBiyBiyBiyBhsBiyBhsBgmBgmBffBffBdZBcTBbNBbNBbNB`BB_;B^5B\)B\)BXBXBXBXBXB[#B\)B\)B`BBe`Be`BdZBbNB^5B]/B^5B_;B^5B]/BYBYBZBZBZBYBYBYBYBXBW
BR�BQ�BS�BS�BS�BT�BO�BQ�BM�BC�B@�B>wB9XB7LB49B2-B0!B/B-B,B)�B(�B)�B-B(�B'�B)�B(�B(�B)�B+B,B-B-B.B.B.B.B0!B1'B1'B2-B2-B33B33B49B5?B8RB5?B6FB8RB7LB33B0!B0!B1'B33B49B5?B7LB;dBA�BB�BD�BH�BI�BI�BJ�BJ�BM�BM�BR�BT�BT�BT�BVBW
BZB\)B^5B^5B^5B_;B_;B]/B^5B`BBdZBgmBgmBiyBk�Bn�Bt�Bu�Bv�Bx�B{�B}�B~�B�B�%B�1B�=B�PB�hB�uB��B��B��B��B��B��B�B�!B�3B�3B�3B�?B�FB�RB�qBÖBǮBɺBɺB��BɺB��B��B�B�B�#B�;B�HB�TB�mB�sB�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	B	%B	+B	
=B	VB	hB	oB	{B	{B	{B	�B	�B	 �B	!�B	"�B	#�B	$�B	&�B	'�B	,B	/B	0!B	2-B	5?B	7LB	:^B	<jB	>wB	?}B	?}B	@�B	D�B	G�B	H�B	I�B	M�B	Q�B	R�B	T�B	W
B	W
B	YB	[#B	]/B	^5B	bNB	e`B	iyB	iyB	k�B	m�B	p�B	r�B	s�B	s�B	s�B	r�B	p�B	o�B	p�B	u�B	x�B	y�B	z�B	|�B	|�B	}�B	~�B	�B	�B	�B	�B	�B	�+B	�1B	�1B	�=B	�JB	�PB	�VB	�bB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�-B	�3B	�3B	�3B	�9B	�9B	�LB	�RB	�dB	�jB	�qB	��B	��B	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�)B	�/B	�/B	�5B	�;B	�;B	�HB	�TB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
+B
1B
	7B
DB
JB
JB
JB
JB
JB
PB
VB
\B
\B
bB
bB
bB
bB
bB
hB
hB
oB
oB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
,B
-B
-B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
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
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
6FB
6FB
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
8RB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
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
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
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
I�B
J�B
J�B
K�B
K�B
L�B
L�B
M�B
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
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
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
[#B
\)B
\)B
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
^5B
^5B
]/B
^5B
^5B
_;B
_;B
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
aHB
aHB
aHB
aHB
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
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
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
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B��B��B��B�}B�}B��B��BBB��B��B��BBBBBÖBBBÖBÖBÖBÖBĜBŢB��B��B�0B�}BѝB�B�	B��B�XB�B��B�B�B�cB�B��B��B��B�$B�2B�}B�EB�3B�B�'B��B�MB��B��B��B��B��B��B�JB��B�B��B�UBq�BcnB]/BZkBO�BI�BFYB;�B,qB;B
�BmBUB��B�3B�B�B��B�B�}B��B�zB��B��B�B��Bw�Bm]B\�BM�BB�B7LB)�B!HB�B�B
XB�B
��B
��B
��B
ŢB
��B
��B
�B
�xB
��B
��B
l"B
i�B
h
B
fB
cTB
`�B
Z�B
R�B
E�B
=�B
.}B
'B
�B
 B
)B	�qB	��B	�B	��B	�-B	өB	�fB	��B	��B	�aB	�B	��B	�)B	�NB	�B	�RB	�gB	}VB	tnB	l�B	aB	W�B	E�B	@�B	:�B	1�B	)B	 �B	�B�DB��B�KB�B�nB�jBیB��B�EB�B��B�B��B��BߊB��B�EB��BбB�B��B�?B�{B�[B�B��B��B��B��B�eB��B�B�)B��B�`B��B��B��B��B�dB�~B��B��B��B��B��B�1B�1B�B��B~]By�BwLBv�Bu�Br�Bq�BoiBmwBj�Bi�BjBi�Bi*Bj0Bh�Bg�Bg�BgBgBd�Bc�Bb�Bb�Bb�B`�B`B_!B]B\�BXyBX�BX�BX�BX�B[�B]B]�Bb4BfLBfLBfBd&B_�B^OB_�B`BB_�B^�BZQBZ�BZkBZ�BZQBYeBYBY�BY�BYeBYKBUMBS[BT�BT{BT�BV�BQ4BS�BN�BDMBA�B?}B9�B7�B4�B33B1�B0!B.IB-]B*B)yB*�B.�B)�B(�B*�B*B*0B+QB+�B,�B-�B-�B.�B.cB.}B.�B0�B1�B2|B4B3�B3�B3�B5B6B9	B6B72B9rB8�B4TB0�B0�B1�B3�B4nB5�B7�B<jBB[BC{BEBIBI�BI�BKxBLBN�BN�BS@BUBU2BU�BV9BW?BZQB\�B^�B^�B^�B`B`B]�B_!Ba-Bd�Bg�Bg�Bi�BlBoiBu%BvFBwBy>B|B~(B}B��B��B��B��B��B��B��B�B�	B�B�B�:B��B��B�UB�MB�MB�hB�tB��B��B��BðB��B��B�XB�DB�#B�dBуB�9B�1B�=B�pB�B�B�B��B�B��B��B�-B�B�%B�	B�B�"B�BB�(B	 4B	;B	-B	MB	B	%B	zB	
�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	# B	$B	%,B	'RB	(sB	,qB	/5B	0UB	2aB	5ZB	7�B	:�B	<�B	>�B	?�B	?�B	@�B	D�B	G�B	H�B	I�B	M�B	R B	S@B	UB	W$B	W?B	YeB	[qB	]dB	^5B	bhB	ezB	iyB	i�B	k�B	m�B	p�B	r�B	s�B	s�B	tB	sMB	p�B	o�B	poB	u�B	y	B	z*B	{B	|�B	}"B	~(B	.B	�UB	�3B	�3B	�9B	�mB	�_B	�fB	�fB	�XB	�~B	�jB	�pB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�,B	�fB	�DB	�B	�B	�B	�B	�iB	��B	�aB	�hB	�MB	��B	��B	��B	�LB	�lB	�B	��B	��B	��B	��B	��B	żB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�[B	�,B	�B	�B	�B	�?B	�EB	�eB	�kB	�]B	�IB	�IB	�jB	�VB	�pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�.B
 4B
AB
-B
-B
GB
SB
YB
EB
KB
	RB
xB
JB
dB
dB
dB
~B
�B
�B
�B
�B
�B
bB
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
&B
&B
'8B
($B
(
B
(
B
)B
)*B
)*B
)*B
*KB
+B
+B
+B
+B
+B
+B
+B
+6B
,B
,"B
,"B
,"B
,"B
,"B
,"B
-)B
-]B
.IB
/5B
/OB
0UB
0UB
0;B
1AB
1AB
2aB
2GB
2GB
2GB
2GB
3hB
3hB
4nB
4TB
5ZB
5tB
5ZB
6zB
6`B
6zB
6`B
6FB
6`B
6FB
6`B
7LB
6FB
6`B
6`B
6`B
7�B
7�B
7�B
7fB
7fB
7fB
7�B
8�B
8lB
8�B
9�B
:xB
:xB
;dB
;dB
;dB
;dB
;dB
;B
;dB
;dB
;B
;dB
;B
;�B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?}B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
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
IB
I�B
J�B
J�B
K�B
K�B
MB
MB
N"B
N�B
O�B
O�B
O�B
O�B
PB
Q B
Q B
Q B
Q B
Q B
R B
R:B
R B
S@B
TB
TB
T,B
UB
U2B
T�B
T�B
UB
UB
VB
VB
V9B
VB
VB
W$B
W$B
W$B
X+B
X+B
X+B
XB
X+B
XEB
YKB
YB
Y1B
YB
Y1B
Z7B
Z7B
Z7B
Z7B
[=B
[=B
[=B
[=B
\)B
[=B
\)B
\CB
\)B
\)B
\CB
\CB
\)B
\)B
\CB
\CB
]dB
]dB
]IB
]IB
]dB
]IB
^5B
^5B
]IB
^OB
^jB
_VB
_VB
_pB
_VB
_pB
_;B
_VB
_;B
_pB
`BB
`\B
`\B
`vB
`vB
abB
aHB
abB
abB
b�B
bNB
bhB
bhB
b�B
cnB
cTB
cTB
c�B
cnB
c�B
dtB
dtB
d�B
dtB
d�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
gmB
g�B
h�B
h�B
hsB
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
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
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612060034002016120600340020161206003400201806221217402018062212174020180622121740201804050410422018040504104220180405041042  JA  ARFMdecpA19c                                                                20161202093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161202003521  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161202003521  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161202003522  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161202003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161202003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161202003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161202003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161202003523  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161202003523                      G�O�G�O�G�O�                JA  ARUP                                                                        20161202013336                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161202153658  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20161202153658  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20161202153658  CV  LATITUDE        G�O�G�O�A�j                JM  ARCAJMQC2.0                                                                 20161205153400  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161205153400  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191042  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031740  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111517                      G�O�G�O�G�O�                
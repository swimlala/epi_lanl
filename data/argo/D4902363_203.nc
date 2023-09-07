CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-26T00:35:43Z creation;2018-01-26T00:35:49Z conversion to V3.1;2019-12-19T07:50:54Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180126003543  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_203                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�Gs��݀1   @�GtUUU�@:�C�\���dWV�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�C3D׀ D�� D���D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@0��@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
Bp�B�
B'�
B/�
B7�
B?�
BG�
BP=qBW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C8\C:\C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD}�D}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�A�D�~�D׾�D���D�;�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D���D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D��D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��jA��!A��hA��DA��7A��A�~�A�x�A�t�A�n�A�`BA�I�A�-A��#A���A���A��`A��;A��A���A��A���A��A��A��mA���A��FA��A���A��uA�x�A�z�A��A�jA��A���A�jA� �A���A��A���A�~�A�r�A�ffA�ZA�G�A�JA��HA���A�hsA�VA���A�A�A�1A�1A�A���A��^A��FA���A�jA�C�A�bNA��A�K�A��A��wA�(�A�t�A�bA�7LA�I�A�|�A�v�A�A��yA���A�ĜA�|�A�VA���A�7LA�A���A�;dA���A�{A��A��+A���A��A�&�A��#A��\A���A�9XA�ffA��RA
=A|^5AzȴAz$�Ayp�AxbNAtE�Aq��ApȴAo�An  Al�Ak�Aj�Ai�Ah�Ah1Ae�TAd�!AdJAc�A`ĜA^M�A^JA]�A]dZA\�uA[A[/AY�AY%AX�DAX  AW�AV�AV{AU�ATz�AS�ASp�ASAR�`AR�DARbAQ�FAQ|�AQ`BAQ�AP-AN^5AL~�AK��AKƨAKp�AJv�AIO�AG��AF�`AFr�AE��AD�`AD5?AC�ABQ�AA��AA��AA7LA@�jA@M�A@�A??}A>ffA=?}A<r�A<�A;�mA;�hA:�HA:I�A:JA9�7A85?A7�FA6�jA6bA4�A4=qA3�
A3��A3�A1�TA0v�A/��A/;dA.��A-VA+&�A*A)/A(�`A(v�A'�PA&�HA%VA"{A!�wA!��A!��A!�7A!l�A!"�A �/A 1AS�AK�A��A�A�AffA=qA�A��A�RA��AVA�A�A�+AjAQ�A=qA�A��AƨA|�A�A�`AbNA��A?}A
�A
1'A	�7A�A��A�^A�A1'AG�A��A  A/A�\A�TA�7AXA ��A A�@���@��u@�`B@�bN@�1'@���@��R@��h@�w@�M�@�`B@�1'@�V@�?}@�b@�@�Z@�\@�@��;@��@�9@�I�@��@ߍP@�-@��/@�  @��@؋D@�t�@��#@��/@ԋD@��y@щ7@�&�@�Ĝ@�  @Χ�@�G�@˝�@���@�?}@�J@�%@�z�@��
@��@�{@�Q�@�C�@���@�$�@��7@�G�@��@��F@�-@��`@�1@���@��u@�;d@�n�@�/@��;@�dZ@�@�-@�`B@�ƨ@���@�{@���@��7@�x�@�7L@�I�@�+@�@�O�@�&�@�%@��`@��9@��@�j@�Z@��@��;@��w@��@�K�@�;d@�+@�"�@���@��H@���@�n�@��@�`B@��@���@��@���@��@��@�9X@�t�@�^5@���@���@���@�@�-@�=q@���@���@�(�@��m@��;@��@�b@��w@���@���@���@�x�@��T@�@�$�@���@��`@��F@���@�?}@�Z@�A�@��m@��@�+@�
=@��@�v�@�5?@��@���@���@�\)@���@���@��+@��@��7@�%@��9@�A�@�1@���@��F@���@�|�@�C�@�@��H@�ȴ@�ȴ@���@���@�~�@�J@��@���@��-@��7@�p�@�7L@�j@�ƨ@�t�@�
=@���@��R@���@���@�z�@��@�z�@��@���@�K�@��R@��^@�x�@�X@���@��D@�P@l�@l�@|�@�P@�;@�@~��@~V@~V@}/@|z�@|�@{"�@y��@y�@y�@y�#@y��@yhs@x��@w�;@w;d@v�@vE�@u`B@uV@t�@t�j@t9X@s�
@s�
@s�
@s�
@s�F@st�@sS�@sC�@so@r�@r��@r~�@r�\@r��@s@s@r^5@rJ@rJ@q�#@qhs@p��@p��@p �@o;d@n�+@m�@mO�@l�@l�@l��@lZ@l9X@l1@k��@k��@k��@k�
@k�
@k�
@k�F@kt�@kC�@ko@j�\@i��@iG�@h�u@h1'@h  @h  @h  @h  @g�w@g��@g�P@g|�@gl�@gK�@gK�@gK�@gK�@f��@f�@f�R@f��@f�+@fv�@f5?@e�@d��@c��@b�H@b�H@b��@b��@b�\@b~�@b^5@bJ@ahs@`�`@`�u@`  @^��@^E�@]��@]��@]��@]�h@]��@]�@]O�@]V@\�j@\z�@\Z@\1@[�@[C�@[33@["�@Z�@Z��@Z-@Y�^@Yhs@YG�@X�`@X�9@X�@X  @W�;@W|�@V�y@V�+@U�-@T�/@T��@Tz�@Tj@T9X@S�
@S�F@So@R�\@R~�@Rn�@Q��@QG�@P�9@PQ�@Pb@O�@O��@O��@O|�@OK�@Nȴ@Nv�@NE�@M�T@M�@L��@Lj@L1@K��@Kƨ@K��@KC�@K@J��@J�\@J~�@J^5@JJ@Ihs@H��@HbN@H �@H �@G�w@G��@G+@Fȴ@Fv�@E��@EV@Dz�@D9X@D9X@D(�@D�@D1@C��@C�m@C�m@C�F@C�@B�@B��@B~�@B�@A��@A��@A��@A��@A�7@Ax�@A%@?�;@?\)@>��@>ȴ@>�R@>�+@>E�@=�@=�h@=`B@=`B@<��@;ƨ@;t�@;S�@;@:��@:��@:�!@:��@:M�@9�@9�^@9x�@9G�@97L@9%@8�9@8r�@8bN@81'@7��@7\)@7;d@6�@6ff@6$�@5�T@5�@4�@4z�@4Z@49X@4(�@4�@3��@3�
@3�F@3��@3dZ@3"�@2��@2n�@1��@17L@0��@0�9@0��@0A�@0  @/�;@/��@/�@/K�@/+@/
=@.�y@.v�@.5?@-�@-@-��@-p�@-O�@-�@,�@,�j@,��@,�D@,Z@,(�@+�m@+C�@*�\@*^5@*^5@*^5@*M�@*-@*J@)�^@)G�@(bN@'�@'�w@'�@'�P@'\)@'�@&ȴ@&v�@%��@%�@%O�@%/@$�j@$I�@$�@#��@#�m@#�
@#�F@#t�@#C�@#@"��@"n�@"n�@"n�@"^5@"^5@"M�@"=q@"-@"�@!�@!�#@!�^@!��@!&�@ �`@ ��@ �u@ �u@ r�@ bN@ bN@ Q�@ A�@  �@   @   @�@�@��@��@�w@l�@�y@�R@��@ff@��@�h@`B@O�@/@�j@j@(�@�@1@1@1@1@1@1@1@��@�@��@^5@J@��@��@G�@��@Ĝ@Ĝ@�9@�u@�@Q�@  @�@K�@+@��@�@�@�+@ff@5?@$�@�@�@�-@V@�/@�j@�@�D@9X@(�@(�@�@��@dZ@S�@33@"�@�@��@n�@M�@-@J@��@�^@x�@X@&�@��@Ĝ@r�@1'@  @��@��@|�@;d@+@�@
=@�@ȴ@��@��@V@@{@�T@�-@�h@`B@/@�@��@�/@��@�j@�D@I�@�@�
@�@o@
��@
~�@
n�@
=q@
-@
-@	��@	��@	��@	hs@	G�@	7L@	�@��@�9@�9@�9@��@��@r�@r�@Q�@�@�w@��@�P@|�@l�@\)@;d@�@�y@�@�@ȴ@�R@�+@v�@v�@ff@ff@ff@ff@ff@5?@$�@{@@�@�@�@��@�-@��@��@�h@/@�/@��@�D@j@I�@9X@�@1@�F@dZ1111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��jA��!A��hA��DA��7A��A�~�A�x�A�t�A�n�A�`BA�I�A�-A��#A���A���A��`A��;A��A���A��A���A��A��A��mA���A��FA��A���A��uA�x�A�z�A��A�jA��A���A�jA� �A���A��A���A�~�A�r�A�ffA�ZA�G�A�JA��HA���A�hsA�VA���A�A�A�1A�1A�A���G�O�G�O�A���A�jA�C�A�bNA��A�K�A��A��wA�(�A�t�A�bA�7LA�I�A�|�A�v�A�A��yA���A�ĜA�|�A�VA���A�7LA�A���A�;dA���A�{A��A��+A���A��A�&�A��#A��\A���A�9XA�ffA��RA
=A|^5AzȴAz$�Ayp�AxbNAtE�Aq��ApȴAo�An  Al�Ak�Aj�Ai�Ah�Ah1Ae�TAd�!AdJAc�A`ĜA^M�A^JA]�A]dZA\�uA[A[/AY�AY%AX�DAX  AW�AV�AV{AU�ATz�AS�ASp�ASAR�`AR�DARbAQ�FAQ|�AQ`BAQ�AP-AN^5AL~�AK��AKƨAKp�AJv�AIO�AG��AF�`AFr�AE��AD�`AD5?AC�ABQ�AA��AA��AA7LA@�jA@M�A@�A??}A>ffA=?}A<r�A<�A;�mA;�hA:�HA:I�A:JA9�7A85?A7�FA6�jA6bA4�A4=qA3�
A3��A3�A1�TA0v�A/��A/;dA.��A-VA+&�A*A)/A(�`A(v�A'�PA&�HA%VA"{A!�wA!��A!��A!�7A!l�A!"�A �/A 1AS�AK�A��A�A�AffA=qA�A��A�RA��AVA�A�A�+AjAQ�A=qA�A��AƨA|�A�A�`AbNA��A?}A
�A
1'A	�7A�A��A�^A�A1'AG�A��A  A/A�\A�TA�7AXA ��A A�@���@��u@�`B@�bN@�1'@���@��R@��h@�w@�M�@�`B@�1'@�V@�?}@�b@�@�Z@�\@�@��;@��@�9@�I�@��@ߍP@�-@��/@�  @��@؋D@�t�@��#@��/@ԋD@��y@щ7@�&�@�Ĝ@�  @Χ�@�G�@˝�@���@�?}@�J@�%@�z�@��
@��@�{@�Q�@�C�@���@�$�@��7@�G�@��@��F@�-@��`@�1@���@��u@�;d@�n�@�/@��;@�dZ@�@�-@�`B@�ƨ@���@�{@���@��7@�x�@�7L@�I�@�+@�@�O�@�&�@�%@��`@��9@��@�j@�Z@��@��;@��w@��@�K�@�;d@�+@�"�@���@��H@���@�n�@��@�`B@��@���@��@���@��@��@�9X@�t�@�^5@���@���@���@�@�-@�=q@���@���@�(�@��m@��;@��@�b@��w@���@���@���@�x�@��T@�@�$�@���@��`@��F@���@�?}@�Z@�A�@��m@��@�+@�
=@��@�v�@�5?@��@���@���@�\)@���@���@��+@��@��7@�%@��9@�A�@�1@���@��F@���@�|�@�C�@�@��H@�ȴ@�ȴ@���@���@�~�@�J@��@���@��-@��7@�p�@�7L@�j@�ƨ@�t�@�
=@���@��R@���@���@�z�@��@�z�@��@���@�K�@��R@��^@�x�@�X@���@��D@�P@l�@l�@|�@�P@�;@�@~��@~V@~V@}/@|z�@|�@{"�@y��@y�@y�@y�#@y��@yhs@x��@w�;@w;d@v�@vE�@u`B@uV@t�@t�j@t9X@s�
@s�
@s�
@s�
@s�F@st�@sS�@sC�@so@r�@r��@r~�@r�\@r��@s@s@r^5@rJ@rJ@q�#@qhs@p��@p��@p �@o;d@n�+@m�@mO�@l�@l�@l��@lZ@l9X@l1@k��@k��@k��@k�
@k�
@k�
@k�F@kt�@kC�@ko@j�\@i��@iG�@h�u@h1'@h  @h  @h  @h  @g�w@g��@g�P@g|�@gl�@gK�@gK�@gK�@gK�@f��@f�@f�R@f��@f�+@fv�@f5?@e�@d��@c��@b�H@b�H@b��@b��@b�\@b~�@b^5@bJ@ahs@`�`@`�u@`  @^��@^E�@]��@]��@]��@]�h@]��@]�@]O�@]V@\�j@\z�@\Z@\1@[�@[C�@[33@["�@Z�@Z��@Z-@Y�^@Yhs@YG�@X�`@X�9@X�@X  @W�;@W|�@V�y@V�+@U�-@T�/@T��@Tz�@Tj@T9X@S�
@S�F@So@R�\@R~�@Rn�@Q��@QG�@P�9@PQ�@Pb@O�@O��@O��@O|�@OK�@Nȴ@Nv�@NE�@M�T@M�@L��@Lj@L1@K��@Kƨ@K��@KC�@K@J��@J�\@J~�@J^5@JJ@Ihs@H��@HbN@H �@H �@G�w@G��@G+@Fȴ@Fv�@E��@EV@Dz�@D9X@D9X@D(�@D�@D1@C��@C�m@C�m@C�F@C�@B�@B��@B~�@B�@A��@A��@A��@A��@A�7@Ax�@A%@?�;@?\)@>��@>ȴ@>�R@>�+@>E�@=�@=�h@=`B@=`B@<��@;ƨ@;t�@;S�@;@:��@:��@:�!@:��@:M�@9�@9�^@9x�@9G�@97L@9%@8�9@8r�@8bN@81'@7��@7\)@7;d@6�@6ff@6$�@5�T@5�@4�@4z�@4Z@49X@4(�@4�@3��@3�
@3�F@3��@3dZ@3"�@2��@2n�@1��@17L@0��@0�9@0��@0A�@0  @/�;@/��@/�@/K�@/+@/
=@.�y@.v�@.5?@-�@-@-��@-p�@-O�@-�@,�@,�j@,��@,�D@,Z@,(�@+�m@+C�@*�\@*^5@*^5@*^5@*M�@*-@*J@)�^@)G�@(bN@'�@'�w@'�@'�P@'\)@'�@&ȴ@&v�@%��@%�@%O�@%/@$�j@$I�@$�@#��@#�m@#�
@#�F@#t�@#C�@#@"��@"n�@"n�@"n�@"^5@"^5@"M�@"=q@"-@"�@!�@!�#@!�^@!��@!&�@ �`@ ��@ �u@ �u@ r�@ bN@ bN@ Q�@ A�@  �@   @   @�@�@��@��@�w@l�@�y@�R@��@ff@��@�h@`B@O�@/@�j@j@(�@�@1@1@1@1@1@1@1@��@�@��@^5@J@��@��@G�@��@Ĝ@Ĝ@�9@�u@�@Q�@  @�@K�@+@��@�@�@�+@ff@5?@$�@�@�@�-@V@�/@�j@�@�D@9X@(�@(�@�@��@dZ@S�@33@"�@�@��@n�@M�@-@J@��@�^@x�@X@&�@��@Ĝ@r�@1'@  @��@��@|�@;d@+@�@
=@�@ȴ@��@��@V@@{@�T@�-@�h@`B@/@�@��@�/@��@�j@�D@I�@�@�
@�@o@
��@
~�@
n�@
=q@
-@
-@	��@	��@	��@	hs@	G�@	7L@	�@��@�9@�9@�9@��@��@r�@r�@Q�@�@�w@��@�P@|�@l�@\)@;d@�@�y@�@�@ȴ@�R@�+@v�@v�@ff@ff@ff@ff@ff@5?@$�@{@@�@�@�@��@�-@��@��@�h@/@�/@��@�D@j@I�@9X@�@1@�F@dZ1111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BDBPB�B�B�B�B�B�B�B�B�BuBJB��B+B�B�B�BhBDB�B�B�B�B�B�B�B�B�B�B�B�B�BVB1BB  B+BPBPBuB�B�B�B{B\BVBJBJBPB\B�B�B�B{BbB
=B��B�B��B�+B�{B�1Bo�BbNB:^B�B{B�BǮB�LB��B��B��B��B��B��B��B�VB|�Bq�B_;BB�BbBJB�B
��B
�B
�yB
�HB
��B
ŢB
��B
�'B
��B
�B
t�B
W
B
B�B
6FB
0!B
:^B
6FB
(�B	��B	��B
B	��B
  B	��B	��B	�NB	�sB	�HB	��B	�wB	��B	��B	�dB	��B	�JB	�!B	��B	��B	��B	��B	�oB	�7B	�%B	�7B	�B	{�B	~�B	w�B	n�B	q�B	o�B	o�B	l�B	o�B	jB	ffB	ffB	dZB	bNB	ZB	J�B	<jB	1'B	@�B	B�B	;dB	0!B	'�B	�B	!�B	%�B	�B	�B	�B	hB	JB	bB	bB	\B	PB		7B	1B	  B��B�B�B��B��B�B�yB�fB�mB�HB�B�B��B��B��B��B��B��BƨB�XB�-B�FB�RB�B��B��B��B��B��B��B�uB�JB{�BiyB�VB�uB�oB�hB�VB�7B�By�Bp�B_;B^5B[#B_;BjBiyBW
BQ�BT�BO�BP�BP�BW
B\)B_;B_;B_;B^5B]/B[#BXBVBS�BK�B?}B8RBB�BG�BA�B@�B@�B9XB0!B8RB0!B1'B/B+B,B,B/B0!B'�B(�B �B�B{B"�B+B&�B!�B �B�B�B�B�B�B�B�B\B�BuB�B�B�B"�B&�B&�B#�B�B�B�B�B\B�B{B�B�B{BuB�B�B�B�B�B�B�B�BDB�B#�B!�B �B�B�B#�B-B/B0!B1'B/B'�B%�B(�B.B(�B)�B1'B9XB8RB9XBB�BC�B?}B?}B<jBB�BK�BL�BP�BO�BL�BG�BF�BK�BR�B[#B\)B\)B\)B]/B^5B^5B]/B]/B_;B_;B`BBbNBbNBcTBbNBcTBbNBbNBaHBcTBgmBjBjBjBjBiyBhsBdZBffBp�Bu�Bw�B~�B�DB�\B�PB�uB��B��B��B��B��B��B��B��B��B�B�9B�?B�9B�'B�'B�!B�LB�^B�qBǮBƨB��B��B��B��B��B�B�
B��B�)B�TB�fB�sB�B�yB�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	  B��B	B	DB	PB	VB	\B	bB	\B	VB	uB	�B	�B	�B	#�B	 �B	�B	%�B	,B	0!B	0!B	0!B	2-B	2-B	2-B	9XB	:^B	8RB	;dB	;dB	C�B	F�B	H�B	H�B	J�B	H�B	F�B	I�B	L�B	I�B	K�B	M�B	M�B	R�B	^5B	_;B	^5B	^5B	]/B	^5B	`BB	gmB	k�B	m�B	n�B	s�B	u�B	u�B	w�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�DB	�JB	�bB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�B	�!B	�!B	�!B	�'B	�-B	�3B	�9B	�FB	�^B	�qB	��B	ÖB	ŢB	ŢB	ŢB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	ȴB	��B	��B	��B	�B	�B	�B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�)B	�5B	�BB	�HB	�HB	�HB	�HB	�BB	�BB	�BB	�HB	�NB	�HB	�HB	�ZB	�`B	�`B	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
	7B

=B
	7B
JB
DB
JB
JB
DB
PB
VB
hB
oB
oB
oB
oB
uB
uB
{B
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
�B
 �B
"�B
!�B
!�B
#�B
#�B
#�B
#�B
$�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
)�B
+B
,B
-B
+B
-B
.B
.B
.B
.B
0!B
0!B
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
5?B
5?B
5?B
49B
49B
7LB
8RB
8RB
8RB
8RB
7LB
7LB
6FB
49B
7LB
8RB
9XB
9XB
8RB
9XB
9XB
9XB
8RB
:^B
;dB
;dB
:^B
;dB
=qB
>wB
>wB
>wB
>wB
=qB
>wB
>wB
>wB
?}B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
@�B
A�B
A�B
A�B
A�B
C�B
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
F�B
F�B
F�B
G�B
F�B
F�B
E�B
H�B
H�B
H�B
G�B
I�B
J�B
K�B
K�B
I�B
K�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
M�B
M�B
L�B
J�B
N�B
M�B
N�B
N�B
O�B
O�B
O�B
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
S�B
S�B
R�B
S�B
T�B
T�B
T�B
T�B
S�B
R�B
VB
VB
W
B
W
B
VB
W
B
W
B
W
B
T�B
W
B
XB
XB
XB
XB
XB
XB
XB
YB
XB
YB
XB
XB
YB
XB
XB
XB
XB
XB
YB
ZB
ZB
[#B
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
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
^5B
_;B
_;B
_;B
_;B
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
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
dZB
e`B
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
gmB
hsB
iyB
iyB
iyB
iyB
iyB
hsB
hsB
iyB
iyB
iyB
iyB
jB
iyB
iyB
iyB
jB
iyB
iyB
hsB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
n�1111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B,BjB��BfB�B�B�B BdB�B�B�B�B�B�B�B�B�B�B�B�B�BB�B�B �BzB�B�B�B�B�B�B�B�B�B�B�BB�BgB�B�B{B�B
�B��G�O�G�O�B��B�2B�	Bq[BdtG�O�BqBSB�nB�^B�B��B��B��B��B�B�B��B�(B~Br�B`�BE�BB�G�O�B
��B
��B
�"B
�nB
�<B
�_G�O�B
�B
�;B
�_B
wLB
Z�B
E�B
9>B
1�B
;dB
7�B
,G�O�B	��B
mB
 �B
UB	�JB	�FB	�ZB	�DB	�4B	ՁB	��B	��B	��B	�PB	��G�O�B	�oB	��B	��B	��B	��B	�[B	��B	�+B	��B	��B	}"B	�B	x�B	o�B	raB	poB	p;B	mB	o�B	kB	gB	f�B	d�B	b�B	Z�B	LJB	>�B	3hB	AB	B�B	<B	1�B	)�B	�B	"�B	&�B	�B	�B	�B	�B	PB	 B	�B	B	�B		�B	�B	UB�B�9B��B�?B�+B�AB�eB�8B��B�4BרB��B�:B�B�0BˬB�\B�jBǮB�B�B�fB�$B�UB�&B��B�B��B�TB�~B��B��B~�BmB��B��B��B��B��B��B��B{0Br-Ba�B`\B]~B`�BkBj�G�O�BS�BVSBQ�BR�BRBW�B\�B_pB_�B_pB^�B]~B[�BX�BV�BT�BMBA�B:xBCaBHfBB�BAoBAUB:�B1�B9	B1�B2-B0!B,=B-B-B/�B0�B(�B)�B"B1BSB#�B+6B'�B"�B!�B�B�B �B�B�B~B�B B�B�B_B�BB#�B'RB'mB$ZB�B�BkB�BBB�BKB)B�BFB�B/BqB�B �B�B~B�BPBpB$@B"hB!|B�B�B$�B-�B/�B0�B1�B/�B(�B'B*B/ B*0B+�B2-B:B9XB:^BB�BDB@4B@OB=�BCaBL0BMBQBPBM6BH�BGzBL�BS�B[=B\]B\xB\]B]dB^OB^jB]~B]dB_pB_�B`vBbhBbhBc�Bb�Bc�Bb�Bb�Ba�Bc�Bg�Bj�Bj�Bj�Bj�Bi�Bh�BeBg8Bp�Bu�BxBB�B�vB��B��B�B��B��B��B�B�:B��B�bB�'B��B�B�?B�nB��B��B�'B�B�dB�B��B�B�B�HB�B�MB�aB�SB�sB��BܬB�B�B��B�B��B�B��B��B�B�B�B�	B�B�B�B�"B�.B	 B	 B	 B	 4B�HB	oB	^B	jB	pB	�B	�B	�B	B	�B	�B	�B	 B	$&B	!bB	 vB	&LB	,"B	0UB	0oB	0�B	2|B	2�B	2�B	9�B	:�B	8�B	;�B	;�B	C�B	F�B	H�B	H�B	J�B	IB	G+B	I�B	L�B	J#B	LB	N"B	N<B	S[B	^OB	_VB	^jB	^OB	]~B	^�B	`�B	g�B	k�B	m�B	o B	s�B	u�B	u�B	xB	B	�B	�AB	�GB	�'B	�-B	�3B	�3B	�MB	�9B	�YB	�RB	�DB	�JB	�HB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�*B	�B	�B	�"B	�B	�!B	�!B	�5B	�!B	�;B	�;B	�[B	�GB	�MB	��B	��B	��B	��B	��B	ðB	ŢB	żB	żB	ĶB	żB	ƨB	��B	��B	��B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�G�O�B	�B	�(B	�TB	�B	�+B	�+B	�+B	�B	�EB	�?B	�sB	�KB	�QB	�eB	ٚB	�xB	�jB	�\B	�HB	�HB	�HB	�bB	�\B	�\B	�vB	�|B	�hB	�B	�|B	�tB	�`B	�zB	�zB	�B	�B	�B	�B	�B	��B	��B	��G�O�B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	�B	��B	��B	�B	�B	��B	�B	��B	�B	�B	�B	�6B	�B	�"B	�<B	�B
 B
'B
GB
GB
MB
3B
B
SB
9B
SB
mB
_B
�B
	RB

rB
	�B
dB
xB
~B
~B
�B
�B
�B
�B
oB
oB
oB
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
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
�B
 �B
 �B
�B
 �B
"�B
"B
!�B
#�B
#�B
$&B
$B
%B
'B
(
B
'�B
(
B
(
B
($B
($B
(
B
(
B
(
B
(>B
($B
(>B
*0B
+B
,"B
-)G�O�B
-CB
./B
./B
.IB
.cB
0;B
0UB
0;B
0UB
1[B
2GB
2aB
3MB
3hB
3hB
3hB
3hB
3hB
4TB
4TB
5tB
5tB
5tB
4�B
4�B
7fB
8RB
8RB
8lB
8lB
7fB
7�B
6�G�O�B
7�B
8lB
9XB
9rB
8�B
9rB
9rB
9�B
8�B
:�B
;B
;B
:�B
;�B
=�B
>wB
>wB
>�B
>�B
=�B
>�B
>�B
>�B
?�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
@�B
A�B
A�B
A�B
A�B
C�B
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
F�B
F�B
F�B
G�B
F�B
F�B
E�B
H�B
H�B
H�B
G�B
I�B
J�B
K�B
K�G�O�B
K�B
MB
M�B
M�G�O�B
N�B
N�B
N�B
M�B
M�B
MB
J�B
N�B
NB
N�B
N�B
O�B
O�B
O�B
Q B
Q�B
Q�B
R B
RB
RB
QB
RB
RB
SB
SB
TB
S�B
SB
T,B
UB
UB
UB
T�B
TB
S&B
VB
V9B
W?B
W$B
VB
W
B
W$B
W$G�O�B
W?B
XB
X+B
X+B
XEB
XEB
XEB
X+B
YB
XEB
YB
XEB
X+B
YB
X+B
X+B
X+B
X+B
XEB
Y1B
Z7B
Z7B
[#B
Z7B
[#B
\)B
\CB
\CB
\)B
\CB
\CB
\CB
[=B
^OB
^OB
]IB
^OB
^OB
_VB
_;B
_VB
_VB
_;B
_VB
_VB
^jB
_VB
_VB
_VB
_pB
`\B
a|B
bNB
bhB
bNB
bNB
bhB
bhB
cnB
cnB
dtB
dZB
d�B
dtB
ezB
e`B
e`B
ezB
e`B
ezB
e`B
ezB
d�B
e�B
f�B
g�B
gmB
gmB
g�B
g�B
g�B
g�B
hsB
hsB
hsB
hsB
g�B
h�B
iyB
iyB
iyB
i�B
i�B
hsB
h�B
i�B
iyB
iyB
i�B
jB
iyB
i�B
i�B
j�B
i�B
i�B
h�B
i�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
n�1111111111111111111111111111111111111111111111111111111111441111141111111111111111111411111141111111111141111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111141111111111114111111111111111111111111111111111111111111111411111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111411114111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801300033122018013000331220180130003312201806221236492018062212364920180622123649201804050433252018040504332520180405043325  JA  ARFMdecpA19c                                                                20180126093519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180126003543  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180126003545  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180126003546  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180126003546  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180126003546  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180126003546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180126003546  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180126003546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180126003546  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180126003549  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180126003549                      G�O�G�O�G�O�                JA  ARUP                                                                        20180126005622                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180126153316  CV  JULD            G�O�G�O�F�;�                JM  ARSQJMQC2.0                                                                 20180129000000  CF  PSAL_ADJUSTED_QCC  D܀ G�O�                JM  ARCAJMQC2.0                                                                 20180129153312  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180129153312  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193325  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033649  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                
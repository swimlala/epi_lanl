CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-03-14T15:37:22Z creation;2019-03-14T15:37:26Z conversion to V3.1;2019-12-18T07:16:23Z update;2022-11-21T05:29:15Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ah   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190314153722  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_167                     2C  Dd	NAVIS_A                         0397                            ARGO 011514                     863 @خ��4 1   @خ�o� @<�n��P�d	���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C|\C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�;�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D��D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�{�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�HRD�k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�ĜA�$�A�%A�l�A�$�A��^A�;dA�-A��A��DA��A���A��7A��+A�|�A�^5A�I�A�E�A�?}A�?}A�=qA�33A� �A�JA�1A�A���A��A��mA��`A��#A���A�ȴA�ĜA�A���A��wA��wA��jA��^A��RA��9A��!A��!A��!A��!A���A���A���A���A���A��uA�z�A�r�A�p�A�jA�XA�=qA�-A��A���A�ȴA��RA��A�VA��FA�\)A�  A��hA�K�A�  A���A��hA�=qA�&�A���A���A��;A���A�^5A��A��A��\A��A�M�A���A�ZA�%A�p�A���A��PA�~�A�n�A�/A���A�XA��wA��A}�FA|z�Az�!Ay��Ax~�AwAwhsAw�Au�Ar�9ArAp��ApVAp5?Ap{Ao�hAnbAk�Ai7LAg�
Af��Af^5Ae�
AeXAc��Ab �A`��A_��A^bNA]\)A\�AZ5?AXjAWAVQ�AU"�ATJASK�ARn�AQO�AP~�AOhsAN�yAM��AKƨAJ~�AJM�AJ�AI�AI��AH��AG|�AFADn�AD�AD1AC��AC;dAB�\A@��A@  A?A>5?A=�A=%A<�/A<��A<M�A;�7A8�A7��A6��A6r�A5�A4�uA3"�A2�RA2Q�A21A1�^A1oA/\)A.�`A.M�A-+A)��A)�A(�A(�9A(JA'�FA'&�A%�A$�RA$�A#�
A#dZA#�A"��A"$�A!�TA!p�A ��A�-A/A��A9XA�A�A�`A9XA��A;dA��A�mA�RA�A�AoA��A�uAjA1'A�A��A;dA~�A\)Ap�A33A"�AE�A��AZA�^AdZA&�A
��A
�jA
~�A
�A	+A�!A7LAbA��AdZA��AjA;dAQ�A��A ��A �A r�A �@���@�o@�v�@�%@���@���@��
@�\)@��@��@�D@�@�@@�C�@�5?@�&�@��
@�o@�M�@陚@�X@�D@�ff@��H@�l�@��@ޗ�@��T@�o@��T@ؓu@׍P@�C�@���@� �@�$�@��/@�(�@϶F@�33@·+@�p�@�K�@�G�@���@�ȴ@�M�@��@���@��/@��@��@���@��+@�1@�t�@��@���@��+@�^5@�{@���@�G�@��@�r�@� �@���@��m@��
@�+@�n�@��-@�7L@���@��m@���@��7@���@���@��@�9X@���@�|�@�S�@�
=@�n�@���@�?}@��@�Z@�t�@���@���@�-@�z�@�S�@��H@�E�@��9@��@���@�o@��!@�V@��@��@�@��h@�G�@��@��j@�bN@��
@���@��#@���@��@��@��
@��w@���@�l�@�C�@�+@���@�5?@��@�j@�b@���@��F@���@�`B@��@�I�@��@��
@���@�p�@���@�bN@�1@�-@��-@�X@�Ĝ@��@��@�z�@�j@�b@���@��F@��@�S�@���@��@��^@���@�?}@�V@��@��j@���@�r�@���@�C�@��y@��R@���@�~�@�V@�E�@�@��h@��@�A�@� �@�(�@� �@��@�ƨ@��@���@�t�@�;d@�33@�"�@�o@���@��y@��H@��H@���@���@�v�@�V@�5?@�-@�{@��-@�%@�z�@K�@~ȴ@~$�@}�@}�@}�@}�@}�@}@|9X@{t�@{33@zn�@y�^@y�^@y&�@xĜ@x�@xA�@x1'@x  @wl�@v��@u�T@up�@t�@t��@t�j@tz�@t9X@sƨ@s��@st�@sC�@r�@r��@r^5@r-@q��@q��@q�7@qx�@qG�@q7L@q&�@p��@pĜ@q%@q�@pĜ@p�@pbN@p �@o��@o�P@o|�@o|�@o\)@o\)@o;d@n�y@n�R@n�+@nE�@n@m��@mO�@l��@l9X@k��@j~�@i��@i�@h�@g�w@fff@f5?@f{@f@e��@e�-@e�@d(�@c�
@cdZ@c@b��@b�\@bM�@a��@a��@`��@`bN@_�P@_K�@^�R@^�+@^V@]�T@]�-@]p�@]/@\��@[�
@[��@[�@[@Z��@Z�@Y�7@YX@Y&�@X�`@Xr�@X  @W�P@W;d@V��@V5?@V@U�@U�h@T�@Tz�@TI�@S�F@S@R�\@Rn�@RM�@R=q@Q�@Q��@Q��@Q��@Q�^@Q��@Q�7@Qhs@Qhs@Q7L@P�`@PbN@PA�@O��@O
=@Nff@N$�@N{@N@M�T@M�T@M�@M�T@M��@M��@M@M��@M�@M/@LI�@Kƨ@K�@KdZ@Ko@J�@J��@J~�@J=q@JJ@I��@I�@I�7@I&�@H��@H�@G�;@G\)@G;d@G+@G
=@F��@F�y@F�R@Fv�@Fv�@Fff@FE�@F5?@E�@EV@D�/@D��@Dj@Dj@DZ@D�@C�F@CS�@C@B�@B�!@Bn�@B^5@B-@A�@A��@A�^@A�7@A7L@@��@@�9@@��@@�@@r�@@bN@@ �@?�w@?��@?��@?�P@?|�@?|�@?l�@?\)@?�@>��@>�y@>�@>�@>�@>��@>V@=p�@=V@<�@<�/@<�j@<��@<j@<1@<1@;��@;t�@;"�@;o@:�H@:�!@:�@9��@9hs@8�@8  @7�;@7��@7��@7�w@7�@7��@7|�@7K�@6�R@6��@6v�@65?@5�@5p�@4j@4(�@4�@41@3��@3o@2�H@2^5@2J@2J@1�@1�#@0��@01'@0  @/�@/�@-��@-�@-�@-p�@,��@,j@+��@+C�@+o@*�H@*��@*��@*��@*��@*��@*n�@*=q@*�@)�#@)��@)�^@)7L@)�@(��@(�u@(A�@'�@'l�@'K�@'+@&��@&��@&ff@%�@%@%��@%p�@%p�@%/@$�@$�@$�/@$Z@#��@#t�@#dZ@#o@"�!@"=q@!�^@!��@!X@ ��@ Ĝ@ �u@ bN@ 1'@ b@   @��@�@�@|�@K�@;d@�@��@�R@��@��@v�@5?@{@{@�T@@p�@�@V@�@��@z�@j@I�@�@1@ƨ@��@t�@dZ@C�@33@33@"�@"�@"�@33@33@33@"�@�H@�H@��@��@M�@�@��@��@G�@��@��@�9@r�@b@  @�;@�@l�@�@
=@��@�y@�@�R@��@ff@@��@��@��@@�-@�@O�@��@�@�/@��@I�@9X@(�@1@�m@�F@t�@S�@33@o@�@�H@��@��@��@�\@n�@=q@J@��@��@�7@hs@G�@7L@%@��@�@Q�@ �@  @�@�@�;@�@�P@;d@�@�@
=@
=@�y@ȴ@�R@��@��@v�@5?@�@�-@p�@?}@V@�/@�D@9X@�@t�@"�@@
��@
��@
��@
�!@
�!@
�!@
�\@
-@
�@
�@
�@	��@	��@	��@	X@	&�@	%@��@��@��@��@�9@��@�u@A�@1'@��@��@\)@+@�@
=@
=@
=@
=@
=@��@
=@�y@��@ȴ@�+@ff@ff@V@V@$�@@��@@@��@��@��@��@��@�h@?}@?}@?}@/@�@�@�@�j@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�ĜA�$�A�%A�l�A�$�A��^A�;dA�-A��A��DA��A���A��7A��+A�|�A�^5A�I�A�E�A�?}A�?}A�=qA�33A� �A�JA�1A�A���A��A��mA��`A��#A���A�ȴA�ĜA�A���A��wA��wA��jA��^A��RA��9A��!A��!A��!A��!A���A���A���A���A���A��uA�z�A�r�A�p�A�jA�XA�=qA�-A��A���A�ȴA��RA��A�VA��FA�\)A�  A��hA�K�A�  A���A��hA�=qA�&�A���A���A��;A���A�^5A��A��A��\A��A�M�A���A�ZA�%A�p�A���A��PA�~�A�n�A�/A���A�XA��wA��A}�FA|z�Az�!Ay��Ax~�AwAwhsAw�Au�Ar�9ArAp��ApVAp5?Ap{Ao�hAnbAk�Ai7LAg�
Af��Af^5Ae�
AeXAc��Ab �A`��A_��A^bNA]\)A\�AZ5?AXjAWAVQ�AU"�ATJASK�ARn�AQO�AP~�AOhsAN�yAM��AKƨAJ~�AJM�AJ�AI�AI��AH��AG|�AFADn�AD�AD1AC��AC;dAB�\A@��A@  A?A>5?A=�A=%A<�/A<��A<M�A;�7A8�A7��A6��A6r�A5�A4�uA3"�A2�RA2Q�A21A1�^A1oA/\)A.�`A.M�A-+A)��A)�A(�A(�9A(JA'�FA'&�A%�A$�RA$�A#�
A#dZA#�A"��A"$�A!�TA!p�A ��A�-A/A��A9XA�A�A�`A9XA��A;dA��A�mA�RA�A�AoA��A�uAjA1'A�A��A;dA~�A\)Ap�A33A"�AE�A��AZA�^AdZA&�A
��A
�jA
~�A
�A	+A�!A7LAbA��AdZA��AjA;dAQ�A��A ��A �A r�A �@���@�o@�v�@�%@���@���@��
@�\)@��@��@�D@�@�@@�C�@�5?@�&�@��
@�o@�M�@陚@�X@�D@�ff@��H@�l�@��@ޗ�@��T@�o@��T@ؓu@׍P@�C�@���@� �@�$�@��/@�(�@϶F@�33@·+@�p�@�K�@�G�@���@�ȴ@�M�@��@���@��/@��@��@���@��+@�1@�t�@��@���@��+@�^5@�{@���@�G�@��@�r�@� �@���@��m@��
@�+@�n�@��-@�7L@���@��m@���@��7@���@���@��@�9X@���@�|�@�S�@�
=@�n�@���@�?}@��@�Z@�t�@���@���@�-@�z�@�S�@��H@�E�@��9@��@���@�o@��!@�V@��@��@�@��h@�G�@��@��j@�bN@��
@���@��#@���@��@��@��
@��w@���@�l�@�C�@�+@���@�5?@��@�j@�b@���@��F@���@�`B@��@�I�@��@��
@���@�p�@���@�bN@�1@�-@��-@�X@�Ĝ@��@��@�z�@�j@�b@���@��F@��@�S�@���@��@��^@���@�?}@�V@��@��j@���@�r�@���@�C�@��y@��R@���@�~�@�V@�E�@�@��h@��@�A�@� �@�(�@� �@��@�ƨ@��@���@�t�@�;d@�33@�"�@�o@���@��y@��H@��H@���@���@�v�@�V@�5?@�-@�{@��-@�%@�z�@K�@~ȴ@~$�@}�@}�@}�@}�@}�@}@|9X@{t�@{33@zn�@y�^@y�^@y&�@xĜ@x�@xA�@x1'@x  @wl�@v��@u�T@up�@t�@t��@t�j@tz�@t9X@sƨ@s��@st�@sC�@r�@r��@r^5@r-@q��@q��@q�7@qx�@qG�@q7L@q&�@p��@pĜ@q%@q�@pĜ@p�@pbN@p �@o��@o�P@o|�@o|�@o\)@o\)@o;d@n�y@n�R@n�+@nE�@n@m��@mO�@l��@l9X@k��@j~�@i��@i�@h�@g�w@fff@f5?@f{@f@e��@e�-@e�@d(�@c�
@cdZ@c@b��@b�\@bM�@a��@a��@`��@`bN@_�P@_K�@^�R@^�+@^V@]�T@]�-@]p�@]/@\��@[�
@[��@[�@[@Z��@Z�@Y�7@YX@Y&�@X�`@Xr�@X  @W�P@W;d@V��@V5?@V@U�@U�h@T�@Tz�@TI�@S�F@S@R�\@Rn�@RM�@R=q@Q�@Q��@Q��@Q��@Q�^@Q��@Q�7@Qhs@Qhs@Q7L@P�`@PbN@PA�@O��@O
=@Nff@N$�@N{@N@M�T@M�T@M�@M�T@M��@M��@M@M��@M�@M/@LI�@Kƨ@K�@KdZ@Ko@J�@J��@J~�@J=q@JJ@I��@I�@I�7@I&�@H��@H�@G�;@G\)@G;d@G+@G
=@F��@F�y@F�R@Fv�@Fv�@Fff@FE�@F5?@E�@EV@D�/@D��@Dj@Dj@DZ@D�@C�F@CS�@C@B�@B�!@Bn�@B^5@B-@A�@A��@A�^@A�7@A7L@@��@@�9@@��@@�@@r�@@bN@@ �@?�w@?��@?��@?�P@?|�@?|�@?l�@?\)@?�@>��@>�y@>�@>�@>�@>��@>V@=p�@=V@<�@<�/@<�j@<��@<j@<1@<1@;��@;t�@;"�@;o@:�H@:�!@:�@9��@9hs@8�@8  @7�;@7��@7��@7�w@7�@7��@7|�@7K�@6�R@6��@6v�@65?@5�@5p�@4j@4(�@4�@41@3��@3o@2�H@2^5@2J@2J@1�@1�#@0��@01'@0  @/�@/�@-��@-�@-�@-p�@,��@,j@+��@+C�@+o@*�H@*��@*��@*��@*��@*��@*n�@*=q@*�@)�#@)��@)�^@)7L@)�@(��@(�u@(A�@'�@'l�@'K�@'+@&��@&��@&ff@%�@%@%��@%p�@%p�@%/@$�@$�@$�/@$Z@#��@#t�@#dZ@#o@"�!@"=q@!�^@!��@!X@ ��@ Ĝ@ �u@ bN@ 1'@ b@   @��@�@�@|�@K�@;d@�@��@�R@��@��@v�@5?@{@{@�T@@p�@�@V@�@��@z�@j@I�@�@1@ƨ@��@t�@dZ@C�@33@33@"�@"�@"�@33@33@33@"�@�H@�H@��@��@M�@�@��@��@G�@��@��@�9@r�@b@  @�;@�@l�@�@
=@��@�y@�@�R@��@ff@@��@��@��@@�-@�@O�@��@�@�/@��@I�@9X@(�@1@�m@�F@t�@S�@33@o@�@�H@��@��@��@�\@n�@=q@J@��@��@�7@hs@G�@7L@%@��@�@Q�@ �@  @�@�@�;@�@�P@;d@�@�@
=@
=@�y@ȴ@�R@��@��@v�@5?@�@�-@p�@?}@V@�/@�D@9X@�@t�@"�@@
��@
��@
��@
�!@
�!@
�!@
�\@
-@
�@
�@
�@	��@	��@	��@	X@	&�@	%@��@��@��@��@�9@��@�u@A�@1'@��@��@\)@+@�@
=@
=@
=@
=@
=@��@
=@�y@��@ȴ@�+@ff@ff@V@V@$�@@��@@@��@��@��@��@��@�h@?}@?}@?}@/@�@�@�@�j@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�jB�ZB�NB�)B��B��BǮBĜBÖB�}B�jB�jB��B��B��B�}B�}B�}B�}B�}B�wB�wB�wB�wB�wB�wB�qB�qB�qB�qB�qB�qB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�dB�dB�^B�^B�^B�^B�RB�LB�FB�FB�9B�-B�!B�B�B��B��B��B�uB�oB�uB��B�oB�JB�B�B}�Bv�BW
B(�B1B�B��B�JB|�B`BBH�B@�B49B'�B#�B�BbB1B
�B
�BB
��B
�jB
�3B
�B
��B
�DB
w�B
n�B
aHB
YB
Q�B
M�B
J�B
G�B
@�B
/B
)�B
"�B
�B
�B
�B
�B
VB	��B	�B	�mB	�NB	�/B	�B	��B	��B	��B	�dB	�9B	�B	��B	��B	��B	�JB	�1B	�B	y�B	t�B	p�B	k�B	e`B	_;B	XB	R�B	J�B	;dB	49B	2-B	33B	2-B	1'B	.B	)�B	$�B	�B	�B	�B	�B	�B	{B	PB	1B	B��B��B��B��B��B��B�B�NB�BB�/B�#B�B�B��B��B��B��B��BɺBĜBB�wB�^B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B�oB�hB�bB�PB�DB�7B�+B�+B�%B�B�B�B~�B|�Bz�Bx�Bw�Bs�Bo�Bm�Bl�Bk�Bk�BjBiyBgmBffBcTBaHB]/B\)B[#BYBVBT�BS�BR�BR�BR�BQ�BP�BO�BM�BL�BJ�BI�BI�BH�BG�BF�BE�BC�BB�BA�BA�B@�B@�B?}B>wB>wB=qB<jB<jB;dB:^B:^B9XB8RB7LB7LB6FB5?B5?B49B49B49B49B33B33B2-B1'B2-B49B33B33B2-B49B33B49B49B33B33B49B49B5?B6FB6FB6FB7LB7LB8RB;dB<jB=qB>wB>wB=qB>wB@�BC�BD�BE�BK�BK�BM�BM�BM�BM�BN�BO�BO�BP�BQ�BR�BR�BR�BQ�BS�BT�BW
BW
BYBZB]/B`BBbNBbNBbNBcTBdZBffBffBgmBhsBjBl�Bn�Bn�Br�Bs�Bt�Bu�Bz�B~�B� B�B�7B�DB�JB�\B�bB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�3B�?B�FB�FB�LB�LB�^B�jB�jB��BBBƨB��B��B��B��B�B�B�#B�/B�5B�5B�;B�HB�`B�`B�mB�mB�sB�B��B��B��B��B��B��B��B��B��B	B		7B	DB	JB	PB	PB	VB	VB	bB	oB	�B	�B	�B	�B	�B	 �B	#�B	&�B	(�B	+B	,B	,B	-B	-B	.B	.B	.B	/B	/B	0!B	0!B	1'B	1'B	1'B	2-B	49B	8RB	:^B	?}B	B�B	F�B	H�B	H�B	H�B	I�B	I�B	J�B	M�B	M�B	N�B	O�B	O�B	O�B	Q�B	R�B	S�B	VB	VB	VB	W
B	YB	[#B	]/B	_;B	`BB	`BB	aHB	bNB	dZB	dZB	e`B	ffB	gmB	hsB	iyB	jB	k�B	l�B	m�B	m�B	n�B	n�B	n�B	o�B	r�B	v�B	x�B	z�B	|�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�7B	�DB	�DB	�PB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�FB	�RB	�^B	�dB	�qB	�wB	�}B	��B	��B	B	ÖB	ĜB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�5B	�;B	�BB	�HB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
	7B

=B
DB
PB
VB
VB
VB
\B
hB
hB
oB
oB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
$�B
$�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
,B
.B
.B
.B
.B
/B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
49B
49B
5?B
5?B
6FB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
<jB
<jB
=qB
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
>wB
@�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
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
T�B
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
T�B
VB
VB
VB
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
`BB
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
hsB
hsB
hsB
iyB
iyB
iyB
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
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�jB�,B�@B�;B�EB��BɺB�mB�gB��B�VB��B��B��B��B��B�}B�}B�}B�}B��B��B��B��B��B��B��B��B�qB��B��B��B�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB��B�B�B�^B�^B�xB��B�lB�LB�`B�zB�nB�aB�UB�cB�kB�LB�zB��B�{B�[B�aB�_B�B��B��B��BcByXBZQB,BdB�xB�WB��B��BcBJ=BBAB5�B(�B$�B5B:B^B
�ZB
��B
�oB
��B
�nB
�;B
�mB
��B
y�B
p�B
b�B
ZkB
R�B
NpB
K�B
I�B
C�B
0!B
+B
#�B
B
5B
�B
�B
NB
�B	�-B	�B	� B	�B	�=B	�
B	͟B	�-B	�B	��B	��B	��B	��B	�sB	��B	��B	��B	{0B	u�B	q�B	l�B	f�B	`�B	YB	T�B	MB	<�B	4�B	2�B	3�B	2�B	2aB	0B	+�B	&�B	;B	B	dB	xB	�B	SB	�B		lB	B��B��B�dB�^B��B�fB�B�B�|B�B�]BںB׍BЗB�vB�PB�~B��B˒BňB��B��B��B��B�}B��B��B��B��B��B�:B��B�)B�=B�B�$B�SB�B�:B��B�pB��B��B��B��B��B�%B��B��B�B~B|Bz�By�Bu�Bp�BnBl�Bk�Bk�BkBjBh>Bg�BeBc:B]�B\�B\xBZ�BW
BU�BT{BS[BS[BSuBRoBQ�BQNBN�BN�BLBJrBJ=BI�BH�BH1BF�BD�BC{BBBA�BA BAB@B?HB?�B>�B=�B=qB;�B:�B;0B:^B9XB8�B8lB6�B6B6+B5%B4�B4�B4�B3�B49B4B3�B4B4�B3�B49B3�B5?B49B4�B4�B4nB4�B5�B5%B5�B6�B6�B6�B8RB8�B9�B<PB=<B=�B>�B>�B>�B?�BBBDgBE�BG+BLJBL0BNBNBNBN"BO\BP.BPHBQhBRTBS&BS&BS@BR�BT�BU�BW�BW�BY�B[#B]�B`�Bb�Bb�Bb�Bc�Bd�Bf�Bf�Bg�BiBkBmBo BoOBs3BtButBwB{�B}B��B�-B��B��B��B��B��B��B��B��B��B��B��B�
B��B�7B��B�|B��B�_B�=B�"B�)B�CB�IB�cB�iB��B��B�B��B��B��B��B�B�dB��B��B��B��B�aBǮB�HB�oBԕB�BٚBچBۦB�~B�OB�OB�pB�B�zB�B�B�B��B�B��B��B�+B��B�B�B�	B�DB�B	�B		�B	xB	dB	jB	�B	�B	�B	�B	&B	�B	�B	�B	�B	�B	 �B	#�B	'B	)*B	+6B	,"B	,=B	-)B	-)B	./B	.IB	./B	/OB	/5B	0oB	0UB	1AB	1AB	1[B	2�B	4�B	8�B	:�B	?�B	B�B	F�B	H�B	H�B	H�B	I�B	I�B	K^B	N"B	NB	O(B	PB	O�B	P.B	R B	SB	TB	VB	V9B	VSB	WYB	YeB	[qB	]~B	_pB	`\B	`\B	a|B	b�B	dtB	dtB	ezB	f�B	g�B	h�B	i�B	j�B	k�B	l�B	m�B	m�B	n�B	n�B	n�B	o�B	r�B	v�B	x�B	z�B	}B	}"B	.B	�'B	�'B	�-B	�-B	�-B	�3B	�9B	�?B	�EB	�KB	�RB	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�FB	�>B	�B	�=B	�IB	�5B	�;B	�UB	�[B	�aB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	� B	�:B	�&B	�,B	�9B	�?B	�KB	�7B	�=B	�qB	�xB	�jB	�VB	�vB	�B	�B	�zB	�B	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�"B	�B	��B	�B	�BB	�.B
 4B
;B
;B
aB
9B
9B
9B
9B
SB
9B
YB
+B
_B
EB
EB
zB
	lB

XB
^B
jB
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
 �B
!�B
#B
"�B
"�B
$&B
$B
%B
%,B
'B
($B
'�B
'�B
)B
(�B
)B
)B
)B
)*B
*B
+B
+B
+6B
+QB
,WB
./B
./B
./B
.IB
/iB
0;B
0UB
1AB
2-B
2GB
2GB
2|B
4nB
4TB
5tB
5�B
6�B
9rB
9XB
9�B
9�B
:�B
:�B
;B
<�B
<�B
=qB
=qB
=qB
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
>�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
NB
NB
N�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
PB
PB
O�B
O�B
O�B
Q B
Q B
P�B
Q B
Q B
Q B
R B
RB
RB
R B
RB
Q�B
RB
SB
SB
SB
TB
TB
S�B
T,B
S�B
T�B
TB
T�B
T�B
T�B
T�B
T�B
U2B
UB
T�B
U2B
UB
UB
VB
VB
VSB
W?B
W$B
X+B
XEB
X+B
X+B
YB
Y1B
Y1B
Y1B
ZQB
ZB
Z7B
ZB
ZB
Z7B
[=B
[WB
[WB
\]B
\CB
\)B
\)B
\CB
\CB
]IB
]IB
]IB
]IB
]dB
^OB
^OB
^5B
^OB
^OB
_VB
_VB
_VB
`\B
`\B
`vB
`BB
`BB
`BB
`vB
`BB
abB
a|B
a|B
a|B
bhB
bhB
b�B
bhB
bhB
bhB
c�B
c�B
cnB
cnB
dZB
dZB
dZB
d�B
dtB
dtB
ezB
ezB
e`B
e`B
f�B
f�B
f�B
ffB
f�B
ffB
f�B
f�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
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
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903250031072019032500310720190325003107202211182138202022111821382020221118213820201903260014462019032600144620190326001446  JA  ARFMdecpA19c                                                                20190315003638  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190314153722  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190314153724  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190314153725  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190314153725  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190314153725  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190314153726  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190314153726  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190314153726                      G�O�G�O�G�O�                JA  ARUP                                                                        20190314155605                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190314153129  CV  JULD            G�O�G�O�F�t�                JM  ARCAJMQC2.0                                                                 20190324153107  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190324153107  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190325151446  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231518                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123820  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                
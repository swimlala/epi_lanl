CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-08-05T18:40:55Z creation;2022-08-05T18:40:56Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220805184055  20220805185635  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�����J1   @��1~K@.ɺ^5?}�c�
=p��1   GPS     A   B   B   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B���B�  B�  B�  B���B���B�  B�  B�  B�  C   C  C  C33C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>ffC?��CB�CC�fCF  CH  CJ  CL  CM�fCO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx�Cz�C|�C}�fC�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�C3Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @#�
@}p�@��R@��RA\)A?\)A_\)A\)A��GA��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BO�
BX=pB_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B�Q�B��B��B׸RB��B��B��B�RB�RB��B��B��B��B��C��C��C(�C��C	��C��C�)C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C>\)C?CB]CC�)CE��CG��CI��CK��CM�)CO�)CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Cl]Cn]Co��Cq��Cs��Cu��Cx]Cz]C|]C}�)C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(��D(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�A�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�_A��A�fA��A�fA�
	A��A�DA�(A��A�A�.A��A�A�A�@A��A�A�{A���A���A��AӼ6Aӹ�AӸRAӲaAӱ'AӰ!AӬ�Aө*AӤ@AӠ�AӝIA�{A�a�A�IRA���Aɯ�A�+kA�ӏA��aA�S�A�Aü�A�o A�P}A�X�A��.A�P�A�oA��FA�H�A��A���A��+A��BA�v+A��A���A��A�<A���A�#�A�YKA�8RA�#A��TA�6A��$A�:*A�JXA��?A��A�?A���A�{�A���A��	A���A��A�G�A���A���A���A�2�A���A��A�S�A��hA��=A��]A�ԕA���A�3hA�(XA�aA�(XA��\A�y�A|��Ax>�Au��An��Aj�
AhW?Ae�Aa��A`��A^��AZ&�AV�cAS(AR4�AP�$AO�oAO�AM˒ALp;AIY�AF�AD	AB��AAVmA@L�A>+A:y�A7C�A5�zA3�	A1�DA1�YA0\)A/�XA.TaA-a|A,[WA+g8A*S�A)xlA(��A(
=A'VA&�A&RTA&(A%��A%m�A%O�A%xA%	A$�)A$c A#�^A# iA"�[A"o A"�A"_A!�A!��A!y�A �[A p�AH�A)�A�yA�zA+A��A��A�A�AAbNAAW�A��AzxA�FA!-A	lA�rA��A3�A�*A�A��A7�A�	A�rAuA��A_�Al�A+kA��AQ�A�&A��Aw�A��A/�A	�A�AqvA�.A�]A��A
�A
��A
~(A
�A	�MA	�.A	�dA	U�A�KA��A��AaA��A��Aq�A�A�A/AߤAm]Ag�A�0A�"Ag8A<�A��A��A!A �QA zxA $@�
�@�8@�ȴ@�?@�A�@�Q�@���@��q@�qv@�S�@���@���@�A�@��#@�a�@��@�{�@��@��@�J#@�q@�=q@��M@��@���@�	@�P�@췀@�p;@��@��@�^5@��@縻@���@�^@�(�@��X@�Ta@㧇@�X@�.@�b�@���@��@�	@ߠ�@�%@ޚ@�e�@�J�@ܔF@�!@�o�@ڋD@ٮ�@ػ�@�R�@�_@׮�@׊	@�o @�Y�@�C�@�%F@�
=@��@�{�@�c@���@�*�@���@��@Ӵ�@��@�@�@�O@��@���@Р�@�[�@��9@�rG@���@�($@��&@͜@�=@̏\@�.�@���@˞�@˄M@�w2@��@��U@ʚ�@�@�hs@�P�@���@�͟@ȆY@�~@��W@�	@ǵt@�O�@��@��@���@ƚ@��z@�Mj@��v@ġb@�1'@���@ìq@�$t@�s�@��@�!�@��}@�E�@��@��@�;@�c @��@��3@��7@�q@�Z@�O@�@���@�E9@��H@�tT@�8�@��W@��d@���@�*0@��`@��@�S�@���@�^�@��R@� �@�ϫ@��k@��K@�V@���@��@�ѷ@�h
@�l�@��@��r@�$@���@�S&@�Z�@���@�k�@��'@��@��[@�&�@���@�ں@�h
@�+k@��#@���@��*@�hs@��@���@���@�z�@�(�@�#:@��@��:@�N<@� \@��[@���@���@�c @�=q@�!@��9@���@�v`@�=�@��)@�_�@���@�w2@�b�@�Q�@�8�@�2a@�&@�
=@���@�6�@�@���@�X@�@���@�{�@�'R@���@���@�^�@�8�@�$t@��8@���@��@�p;@�J�@��@��}@�|�@�/�@��@���@���@�u%@��@���@��7@���@�W�@�D�@�9X@��@��P@�.I@�� @�-@���@���@�Z�@�2a@�"�@��E@��?@���@�|�@�@�x�@�A�@��}@�v�@�bN@�*�@���@�k�@��@��b@�g8@��@���@��{@�j@�,�@��@���@�H@���@���@���@�N<@��p@�z�@�\�@�M@�C�@� �@���@�خ@��H@���@�X�@��@��z@�H�@��+@���@���@���@��@�@O@��@��<@���@�Xy@���@��K@���@�}�@�Mj@��	@���@�~�@�E�@�G@���@��9@�� @�ƨ@���@�G�@��@��,@��@���@�Ft@���@��h@��@�j@��6@�?@�-�@���@���@��	@�7L@��@��@���@��@��O@��x@���@��@�]d@��#@���@��n@��@�-w@���@���@�w�@�1@F�@~u@}�C@~V@~a|@~d�@~)�@}�@}��@}�@{�@{'�@z��@y4@x��@x�@x�@y*0@y4@x�@x:�@w��@wg�@v�B@vq�@vM�@v �@u�S@uQ�@uj@uB�@t|�@s�a@sb�@s9�@s�@r��@r0U@q�@p2�@o�6@o�f@n��@nff@n�@m@m��@m+@lh�@l6@l	�@k�+@k��@k�a@k�	@j��@j}V@i��@i�@i�~@i!�@h�P@h�Y@h!@gH�@fz@f$�@e�d@e?}@d��@d��@dN�@d7@c�&@c��@c$t@b�@b��@a��@a}�@`�j@`�u@`y>@`I�@`7@_�m@_�6@_�K@_�@_A�@^�]@^\�@]�@]L�@\֡@\_@[~�@[�@Z�c@ZQ@YA @X�@X�Y@X"h@W�F@W�4@W�@V��@V��@U�C@U:�@U�@T�[@T��@TU2@T,=@S�@S��@S�k@S�@Rȴ@R}V@R	@Q��@Q��@QQ�@Q/@P��@P��@P~@O�w@OO@O�@N��@M|@Ly>@L	�@Kƨ@K��@K8@K,�@K$t@J��@JGE@I��@I�~@I/@H��@G��@GdZ@F��@F�6@F^5@E�.@EL�@D��@D�_@D-�@C�a@C�V@C�@C@A��@A;@@Z@?l�@?;d@?S@>�@>^5@>e@=�Z@=�9@=��@=��@=N<@<m�@;��@;��@;(@:kQ@:!�@9��@9X@8�E@8PH@8"h@7ƨ@7��@7�q@7��@7��@7qv@7e�@7RT@7Mj@7>�@6ں@61�@5�d@5�3@5��@5`B@4�@4c�@3�&@3��@38@3�@2�]@2��@2=q@1ϫ@1}�@1S&@0�D@/��@/�@@.�8@.B[@-�@-�@.�@-�o@-�@-�@-zx@-O�@-0�@-%F@,��@,�@+�*@+4�@*�M@*��@*��@*}V@*1�@)�@)�C@)s�@)q@)�@(�@(��@(ѷ@(��@(I�@'��@'�f@'�@&��@&�1@&c @&@�@&1�@%�Z@%�@%B�@%IR@%=�@%5�@%%F@%�@$�/@$�j@$��@$ �@#l�@#H�@#1�@#33@#4�@#6z@#/�@#Y@"�@"}V@"E�@"
�@!��@!��@!(�@ ��@ l"@ %�@ $@ 7@ 1@��@��@=@��@�X@�F@p;@0U@
�@��@�@��@rG@-w@�/@��@�4@��@y>@`�@M@1'@��@�
@�V@qv@s@dZ@@�}@�\@�\@p;@YK@�@ԕ@�H@��@�"@hs@c�@[W@S&@�@��@��@l"@<�@�@��@��@A�@S@�}@��@i�@Z�@0U@��@k�@IR@A @0�@	l@;@�/@K^@%�@@�@1@�m@{J@"�@ں@��@W�@B[@
�@��@��@&�@�@��@u�@tT@bN@1'@�A@��@X�@C@�@��@��@V@.�@�@�@�@�.@J@{@�@�9@��@=�@�@	l@�@�@�@��@�v@��@`�@S�@>B@6@/�11111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�_A��A�fA��A�fA�
	A��A�DA�(A��A�A�.A��A�A�A�@A��A�A�{A���A���A��AӼ6Aӹ�AӸRAӲaAӱ'AӰ!AӬ�Aө*AӤ@AӠ�AӝIA�{A�a�A�IRA���Aɯ�A�+kA�ӏA��aA�S�A�Aü�A�o A�P}A�X�A��.A�P�A�oA��FA�H�A��A���A��+A��BA�v+A��A���A��A�<A���A�#�A�YKA�8RA�#A��TA�6A��$A�:*A�JXA��?A��A�?A���A�{�A���A��	A���A��A�G�A���A���A���A�2�A���A��A�S�A��hA��=A��]A�ԕA���A�3hA�(XA�aA�(XA��\A�y�A|��Ax>�Au��An��Aj�
AhW?Ae�Aa��A`��A^��AZ&�AV�cAS(AR4�AP�$AO�oAO�AM˒ALp;AIY�AF�AD	AB��AAVmA@L�A>+A:y�A7C�A5�zA3�	A1�DA1�YA0\)A/�XA.TaA-a|A,[WA+g8A*S�A)xlA(��A(
=A'VA&�A&RTA&(A%��A%m�A%O�A%xA%	A$�)A$c A#�^A# iA"�[A"o A"�A"_A!�A!��A!y�A �[A p�AH�A)�A�yA�zA+A��A��A�A�AAbNAAW�A��AzxA�FA!-A	lA�rA��A3�A�*A�A��A7�A�	A�rAuA��A_�Al�A+kA��AQ�A�&A��Aw�A��A/�A	�A�AqvA�.A�]A��A
�A
��A
~(A
�A	�MA	�.A	�dA	U�A�KA��A��AaA��A��Aq�A�A�A/AߤAm]Ag�A�0A�"Ag8A<�A��A��A!A �QA zxA $@�
�@�8@�ȴ@�?@�A�@�Q�@���@��q@�qv@�S�@���@���@�A�@��#@�a�@��@�{�@��@��@�J#@�q@�=q@��M@��@���@�	@�P�@췀@�p;@��@��@�^5@��@縻@���@�^@�(�@��X@�Ta@㧇@�X@�.@�b�@���@��@�	@ߠ�@�%@ޚ@�e�@�J�@ܔF@�!@�o�@ڋD@ٮ�@ػ�@�R�@�_@׮�@׊	@�o @�Y�@�C�@�%F@�
=@��@�{�@�c@���@�*�@���@��@Ӵ�@��@�@�@�O@��@���@Р�@�[�@��9@�rG@���@�($@��&@͜@�=@̏\@�.�@���@˞�@˄M@�w2@��@��U@ʚ�@�@�hs@�P�@���@�͟@ȆY@�~@��W@�	@ǵt@�O�@��@��@���@ƚ@��z@�Mj@��v@ġb@�1'@���@ìq@�$t@�s�@��@�!�@��}@�E�@��@��@�;@�c @��@��3@��7@�q@�Z@�O@�@���@�E9@��H@�tT@�8�@��W@��d@���@�*0@��`@��@�S�@���@�^�@��R@� �@�ϫ@��k@��K@�V@���@��@�ѷ@�h
@�l�@��@��r@�$@���@�S&@�Z�@���@�k�@��'@��@��[@�&�@���@�ں@�h
@�+k@��#@���@��*@�hs@��@���@���@�z�@�(�@�#:@��@��:@�N<@� \@��[@���@���@�c @�=q@�!@��9@���@�v`@�=�@��)@�_�@���@�w2@�b�@�Q�@�8�@�2a@�&@�
=@���@�6�@�@���@�X@�@���@�{�@�'R@���@���@�^�@�8�@�$t@��8@���@��@�p;@�J�@��@��}@�|�@�/�@��@���@���@�u%@��@���@��7@���@�W�@�D�@�9X@��@��P@�.I@�� @�-@���@���@�Z�@�2a@�"�@��E@��?@���@�|�@�@�x�@�A�@��}@�v�@�bN@�*�@���@�k�@��@��b@�g8@��@���@��{@�j@�,�@��@���@�H@���@���@���@�N<@��p@�z�@�\�@�M@�C�@� �@���@�خ@��H@���@�X�@��@��z@�H�@��+@���@���@���@��@�@O@��@��<@���@�Xy@���@��K@���@�}�@�Mj@��	@���@�~�@�E�@�G@���@��9@�� @�ƨ@���@�G�@��@��,@��@���@�Ft@���@��h@��@�j@��6@�?@�-�@���@���@��	@�7L@��@��@���@��@��O@��x@���@��@�]d@��#@���@��n@��@�-w@���@���@�w�@�1@F�@~u@}�C@~V@~a|@~d�@~)�@}�@}��@}�@{�@{'�@z��@y4@x��@x�@x�@y*0@y4@x�@x:�@w��@wg�@v�B@vq�@vM�@v �@u�S@uQ�@uj@uB�@t|�@s�a@sb�@s9�@s�@r��@r0U@q�@p2�@o�6@o�f@n��@nff@n�@m@m��@m+@lh�@l6@l	�@k�+@k��@k�a@k�	@j��@j}V@i��@i�@i�~@i!�@h�P@h�Y@h!@gH�@fz@f$�@e�d@e?}@d��@d��@dN�@d7@c�&@c��@c$t@b�@b��@a��@a}�@`�j@`�u@`y>@`I�@`7@_�m@_�6@_�K@_�@_A�@^�]@^\�@]�@]L�@\֡@\_@[~�@[�@Z�c@ZQ@YA @X�@X�Y@X"h@W�F@W�4@W�@V��@V��@U�C@U:�@U�@T�[@T��@TU2@T,=@S�@S��@S�k@S�@Rȴ@R}V@R	@Q��@Q��@QQ�@Q/@P��@P��@P~@O�w@OO@O�@N��@M|@Ly>@L	�@Kƨ@K��@K8@K,�@K$t@J��@JGE@I��@I�~@I/@H��@G��@GdZ@F��@F�6@F^5@E�.@EL�@D��@D�_@D-�@C�a@C�V@C�@C@A��@A;@@Z@?l�@?;d@?S@>�@>^5@>e@=�Z@=�9@=��@=��@=N<@<m�@;��@;��@;(@:kQ@:!�@9��@9X@8�E@8PH@8"h@7ƨ@7��@7�q@7��@7��@7qv@7e�@7RT@7Mj@7>�@6ں@61�@5�d@5�3@5��@5`B@4�@4c�@3�&@3��@38@3�@2�]@2��@2=q@1ϫ@1}�@1S&@0�D@/��@/�@@.�8@.B[@-�@-�@.�@-�o@-�@-�@-zx@-O�@-0�@-%F@,��@,�@+�*@+4�@*�M@*��@*��@*}V@*1�@)�@)�C@)s�@)q@)�@(�@(��@(ѷ@(��@(I�@'��@'�f@'�@&��@&�1@&c @&@�@&1�@%�Z@%�@%B�@%IR@%=�@%5�@%%F@%�@$�/@$�j@$��@$ �@#l�@#H�@#1�@#33@#4�@#6z@#/�@#Y@"�@"}V@"E�@"
�@!��@!��@!(�@ ��@ l"@ %�@ $@ 7@ 1@��@��@=@��@�X@�F@p;@0U@
�@��@�@��@rG@-w@�/@��@�4@��@y>@`�@M@1'@��@�
@�V@qv@s@dZ@@�}@�\@�\@p;@YK@�@ԕ@�H@��@�"@hs@c�@[W@S&@�@��@��@l"@<�@�@��@��@A�@S@�}@��@i�@Z�@0U@��@k�@IR@A @0�@	l@;@�/@K^@%�@@�@1@�m@{J@"�@ں@��@W�@B[@
�@��@��@&�@�@��@u�@tT@bN@1'@�A@��@X�@C@�@��@��@V@.�@�@�@�@�.@J@{@�@�9@��@=�@�@	l@�@�@�@��@�v@��@`�@S�@>B@6@/�11111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
�B
��B
�B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�yB
�_B
�DB
�_B
�*B
�*B
�B
��B
�B
�B
��B
� B
�B
��B
�B
�,B
�B
�B
��B
�B
�2B
�B
�B
�B
�B]�Bc�BX�B�eB��B�B
	B"�B:�BJXBQhBZ�Bg�Bl�BtBwfB�[B�B��B�dB��B��B��B�uBv�BkkBb�B]BX�BPbBIlBI�BF?B88B0B%,B�BaB
�BGB��B�6B�xB�_B�VBĜB�vB�,B��Bg�BMPB=�B+B�B�B
�*B
�B
��B
gB
Q B
#�B
�B	�B	�PB	��B	�,B	��B	w�B	lWB	\CB	UgB	L�B	7�B	,�B	$B	!bB	B	�B	�B	�B	�B	�B	
	B	�B	 �B�qB�*B��B�B�wB��B��B�B�ZB�.B	�B	�B	�B	)*B	3MB	9$B	@ B	DB	Z�B	i*B	yXB	��B	�JB	�gB	�B	�BB	��B	�OB	��B	�B	�B	�^B	��B	�B	�B	�{B	յB	��B	چB	߾B	�B	�B	��B	�B	�qB	��B	�5B	��B	�B	�OB	�/B	�B	��B	�}B
�B
 �B	��B	�B	��B	��B
 iB
 4B
�B
aB
�B
'B
AB
aB
mB
�B
�B
�B
tB
�B
�B
{B
�B
�B
�B
 iB	��B
B
�B
�B

�B
�B
	7B
�B
"B
[B
B
sB
�B
�B
�B
�B
_B
EB
eB
�B
eB
�B
�B
yB
+B
�B
�B
�B
�B
�B
�B
�B
�B
�B
6B
~B
xB

�B
	�B
�B
B
	�B

XB

�B

=B
	�B
EB
�B
B
�B
uB
[B
[B
'B
uB
gB
B
�B
�B
�B
 �B
B
�B
B
�B
�B
�B
9B
�B
[B
�B	��B	�qB	��B	��B	��B	��B	��B	�rB	��B	�lB	�B	�lB	�lB	��B	��B	��B	��B	�rB	�RB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�fB	�fB	��B	�	B	��B	�8B	��B	��B	�B	�RB	�lB	�B	�RB	�B	�RB	�lB	�RB	�$B	��B	�	B	�	B	�XB	��B	��B	��B	��B	��B	��B	�^B	��B	��B	�B	�B	�jB	��B	�B	�*B	�B	�BB
;B
�B
�B
B
�B
�B
GB
�B
;B
�B
�B
B
�B
GB
�B
B
�B
mB
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
�B
�B
�B
�B
tB
YB
YB
�B
�B
�B
�B
�B
KB
	B
�B
�B
	lB

�B
^B
)B
�B
�B
^B

�B

rB

#B

�B
)B

�B

�B
0B
0B
0B
�B
6B
"B
BB
�B
�B
 B
NB
:B
oB
B
&B
@B
[B
[B
[B
[B
[B
�B
�B
�B
�B
�B
�B
�B
�B
{B
aB
{B
B
�B
�B
�B
�B
SB
B
mB

B
$B
?B
?B
$B

B
�B
�B
�B
gB
�B
�B

B
�B
�B
�B
?B
�B
�B
B
KB
QB
�B
qB
qB
�B
�B
�B
#B
#B
#B
qB
WB
�B
�B
�B
�B
~B
dB
B
�B
�B
)B
B
IB
�B
�B
�B
B
�B
 \B
 �B
!|B
"hB
"�B
#�B
$B
$@B
%FB
%FB
%FB
%`B
%�B
&2B
&�B
&�B
'B
'8B
'�B
'�B
'�B
(
B
($B
($B
(�B
)B
(�B
)B
)yB
*B
*eB
*B
*�B
*B
*�B
*�B
+B
+B
+B
+QB
+kB
,"B
,WB
,�B
-CB
-CB
-CB
-CB
-�B
./B
./B
.B
.IB
.�B
.�B
.�B
.�B
/B
/�B
/�B
0B
0!B
0�B
0oB
0UB
0!B
0B
0!B
0�B
0�B
1B
1AB
1B
1B
/�B
/�B
/�B
0B
/5B
-�B
-�B
-�B
-�B
.�B
.�B
.IB
.}B
.cB
.�B
/5B
/�B
/�B
/�B
/�B
/�B
/iB
/OB
/OB
.�B
-�B
-wB
-�B
.IB
-wB
-CB
.B
1vB
1�B
2�B
3�B
4B
3�B
3�B
3�B
3MB
2�B
1'B
1�B
2�B
3hB
4nB
4�B
5tB
5tB
5�B
6`B
8B
8RB
8RB
8lB
8�B
9XB
:B
:B
9�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8lB
8lB
8�B
9>B
9rB
9�B
:B
:�B
;B
;�B
<B
<6B
<6B
<�B
=<B
>B
>�B
>�B
>�B
>�B
>�B
>�B
?HB
>�B
>�B
>�B
?�B
?�B
?�B
@B
@4B
@OB
@iB
@iB
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
C-B
CGB
CGB
CaB
C�B
C�B
C�B
D�B
D�B
D�B
E9B
FB
E�B
EmB
E�B
F�B
F�B
F�B
GB
G+B
GEB
G�B
G�B
G�B
HfB
H�B
H�B
H�B
IB
IlB
I�B
I�B
I�B
J	B
JrB
J�B
J�B
KDB
KxB
KxB
K�B
K�B
K�B
K�B
LdB
L�B
L�B
MB
L�B
M�B
NB
N"B
NVB
NpB
OB
OB
OB
OvB
O�B
PHB
PHB
P.B
PB
PHB
P�B
Q4B
QB
QB
Q4B
QNB
Q4B
QB
Q4B
QNB
Q�B
R�B
TFB
T,B
S[B
S&B
SB
R�B
S[B
S�B
TB
T�B
VB
V9B
V9B
VSB
V�B
VmB
W
B
W$B
W�B
XEB
X_B
XEB
X�B
X�B
Y1B
YKB
YeB
YKB
YKB
YKB
YKB
YKB
YeB
YB
Y�B
Y�B
Y�B
ZQB
[�B
\�B
\�B
\�B
\�B
\�B
\�B
^�B
_�B
`'B
`B
`'B
`'B
`�B
`vB
`vB
`�B
`�B
a|B
bhB
b�B
d@B
eB
fB
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
hXB
h�B
h�B
h�B
iB
iB
iB
iB
h�B
h�B
h�B
iB
iB
i�B
jKB
jKB
j�B
kB
k�B
k�B
lB
l=B
lWB
lqB
l�B
l�B
l�B
mwB
m�B
mwB
m�B
mwB
m�B
m�B
m�B
m�B
nB
n�B
oB
oOB
oiB
o�B
o�B
o�B
o�B
pB
poB
p�B
p�B
q'B
qB
q�B
q�B
r-B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
shB
s�B
s�B
s�B
tB
tTB
tTB
tTB
tnB
t�B
t�B
uZB
uZB
utB
utB
u�B
u�B
u�B
u�B
v+B
vB
v`B
v`B
v`B
v`B
v�B
wB
w2B
w2B
wfB
wfB
w�B
w�B
xB
xB
xRB
xlB
xlB
xlB
xRB
x�B
y$B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
z^B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{dB
{B
{�B
|PB
|6B
|B
|B
|B
{�B
|PB
|�B
|�B
}<B
}qB
}qB
}�B
~(B
~�B
B
B
cB
cB
�B
�B
� B
� B
�B
�B
�B
��B
�B
� B
�B
��B
��B
� B
�B
�uB
��B
�uB
��B
�B
�aB
�B
�MB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�9B
�9B
�11111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
�B
��B
�B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�yB
�_B
�DB
�_B
�*B
�*B
�B
��B
�B
�B
��B
� B
�B
��B
�B
�,B
�B
�B
��B
�B
�2B
�B
�B
�B
�B]�Bc�BX�B�eB��B�B
	B"�B:�BJXBQhBZ�Bg�Bl�BtBwfB�[B�B��B�dB��B��B��B�uBv�BkkBb�B]BX�BPbBIlBI�BF?B88B0B%,B�BaB
�BGB��B�6B�xB�_B�VBĜB�vB�,B��Bg�BMPB=�B+B�B�B
�*B
�B
��B
gB
Q B
#�B
�B	�B	�PB	��B	�,B	��B	w�B	lWB	\CB	UgB	L�B	7�B	,�B	$B	!bB	B	�B	�B	�B	�B	�B	
	B	�B	 �B�qB�*B��B�B�wB��B��B�B�ZB�.B	�B	�B	�B	)*B	3MB	9$B	@ B	DB	Z�B	i*B	yXB	��B	�JB	�gB	�B	�BB	��B	�OB	��B	�B	�B	�^B	��B	�B	�B	�{B	յB	��B	چB	߾B	�B	�B	��B	�B	�qB	��B	�5B	��B	�B	�OB	�/B	�B	��B	�}B
�B
 �B	��B	�B	��B	��B
 iB
 4B
�B
aB
�B
'B
AB
aB
mB
�B
�B
�B
tB
�B
�B
{B
�B
�B
�B
 iB	��B
B
�B
�B

�B
�B
	7B
�B
"B
[B
B
sB
�B
�B
�B
�B
_B
EB
eB
�B
eB
�B
�B
yB
+B
�B
�B
�B
�B
�B
�B
�B
�B
�B
6B
~B
xB

�B
	�B
�B
B
	�B

XB

�B

=B
	�B
EB
�B
B
�B
uB
[B
[B
'B
uB
gB
B
�B
�B
�B
 �B
B
�B
B
�B
�B
�B
9B
�B
[B
�B	��B	�qB	��B	��B	��B	��B	��B	�rB	��B	�lB	�B	�lB	�lB	��B	��B	��B	��B	�rB	�RB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�fB	�fB	��B	�	B	��B	�8B	��B	��B	�B	�RB	�lB	�B	�RB	�B	�RB	�lB	�RB	�$B	��B	�	B	�	B	�XB	��B	��B	��B	��B	��B	��B	�^B	��B	��B	�B	�B	�jB	��B	�B	�*B	�B	�BB
;B
�B
�B
B
�B
�B
GB
�B
;B
�B
�B
B
�B
GB
�B
B
�B
mB
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
�B
�B
�B
�B
tB
YB
YB
�B
�B
�B
�B
�B
KB
	B
�B
�B
	lB

�B
^B
)B
�B
�B
^B

�B

rB

#B

�B
)B

�B

�B
0B
0B
0B
�B
6B
"B
BB
�B
�B
 B
NB
:B
oB
B
&B
@B
[B
[B
[B
[B
[B
�B
�B
�B
�B
�B
�B
�B
�B
{B
aB
{B
B
�B
�B
�B
�B
SB
B
mB

B
$B
?B
?B
$B

B
�B
�B
�B
gB
�B
�B

B
�B
�B
�B
?B
�B
�B
B
KB
QB
�B
qB
qB
�B
�B
�B
#B
#B
#B
qB
WB
�B
�B
�B
�B
~B
dB
B
�B
�B
)B
B
IB
�B
�B
�B
B
�B
 \B
 �B
!|B
"hB
"�B
#�B
$B
$@B
%FB
%FB
%FB
%`B
%�B
&2B
&�B
&�B
'B
'8B
'�B
'�B
'�B
(
B
($B
($B
(�B
)B
(�B
)B
)yB
*B
*eB
*B
*�B
*B
*�B
*�B
+B
+B
+B
+QB
+kB
,"B
,WB
,�B
-CB
-CB
-CB
-CB
-�B
./B
./B
.B
.IB
.�B
.�B
.�B
.�B
/B
/�B
/�B
0B
0!B
0�B
0oB
0UB
0!B
0B
0!B
0�B
0�B
1B
1AB
1B
1B
/�B
/�B
/�B
0B
/5B
-�B
-�B
-�B
-�B
.�B
.�B
.IB
.}B
.cB
.�B
/5B
/�B
/�B
/�B
/�B
/�B
/iB
/OB
/OB
.�B
-�B
-wB
-�B
.IB
-wB
-CB
.B
1vB
1�B
2�B
3�B
4B
3�B
3�B
3�B
3MB
2�B
1'B
1�B
2�B
3hB
4nB
4�B
5tB
5tB
5�B
6`B
8B
8RB
8RB
8lB
8�B
9XB
:B
:B
9�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8lB
8lB
8�B
9>B
9rB
9�B
:B
:�B
;B
;�B
<B
<6B
<6B
<�B
=<B
>B
>�B
>�B
>�B
>�B
>�B
>�B
?HB
>�B
>�B
>�B
?�B
?�B
?�B
@B
@4B
@OB
@iB
@iB
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
C-B
CGB
CGB
CaB
C�B
C�B
C�B
D�B
D�B
D�B
E9B
FB
E�B
EmB
E�B
F�B
F�B
F�B
GB
G+B
GEB
G�B
G�B
G�B
HfB
H�B
H�B
H�B
IB
IlB
I�B
I�B
I�B
J	B
JrB
J�B
J�B
KDB
KxB
KxB
K�B
K�B
K�B
K�B
LdB
L�B
L�B
MB
L�B
M�B
NB
N"B
NVB
NpB
OB
OB
OB
OvB
O�B
PHB
PHB
P.B
PB
PHB
P�B
Q4B
QB
QB
Q4B
QNB
Q4B
QB
Q4B
QNB
Q�B
R�B
TFB
T,B
S[B
S&B
SB
R�B
S[B
S�B
TB
T�B
VB
V9B
V9B
VSB
V�B
VmB
W
B
W$B
W�B
XEB
X_B
XEB
X�B
X�B
Y1B
YKB
YeB
YKB
YKB
YKB
YKB
YKB
YeB
YB
Y�B
Y�B
Y�B
ZQB
[�B
\�B
\�B
\�B
\�B
\�B
\�B
^�B
_�B
`'B
`B
`'B
`'B
`�B
`vB
`vB
`�B
`�B
a|B
bhB
b�B
d@B
eB
fB
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
hXB
h�B
h�B
h�B
iB
iB
iB
iB
h�B
h�B
h�B
iB
iB
i�B
jKB
jKB
j�B
kB
k�B
k�B
lB
l=B
lWB
lqB
l�B
l�B
l�B
mwB
m�B
mwB
m�B
mwB
m�B
m�B
m�B
m�B
nB
n�B
oB
oOB
oiB
o�B
o�B
o�B
o�B
pB
poB
p�B
p�B
q'B
qB
q�B
q�B
r-B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
shB
s�B
s�B
s�B
tB
tTB
tTB
tTB
tnB
t�B
t�B
uZB
uZB
utB
utB
u�B
u�B
u�B
u�B
v+B
vB
v`B
v`B
v`B
v`B
v�B
wB
w2B
w2B
wfB
wfB
w�B
w�B
xB
xB
xRB
xlB
xlB
xlB
xRB
x�B
y$B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
z^B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{dB
{B
{�B
|PB
|6B
|B
|B
|B
{�B
|PB
|�B
|�B
}<B
}qB
}qB
}�B
~(B
~�B
B
B
cB
cB
�B
�B
� B
� B
�B
�B
�B
��B
�B
� B
�B
��B
��B
� B
�B
�uB
��B
�uB
��B
�B
�aB
�B
�MB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�9B
�9B
�11111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220805184040  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220805184055  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220805184056  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220805184056                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220806034100  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220806034100  QCF$                G�O�G�O�G�O�            4000JA  ARUP                                                                        20220805185635                      G�O�G�O�G�O�                
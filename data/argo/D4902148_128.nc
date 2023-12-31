CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-02-17T15:36:11Z creation;2018-02-17T15:36:14Z conversion to V3.1;2019-12-18T07:24:52Z update;2022-11-21T05:31:13Z update;     
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
_FillValue                 �  ]d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20180217153611  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_128                     2C  Dd?&NAVIS_A                         0397                            ARGO 011514                     863 @�M��} 1   @�MhK� @<p:�~��d?&���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD��fD�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@�Q�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD��D�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�
D$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�;�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D��D��D�޸1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�bNA�^5A�`BA�dZA�hsA�n�A�t�A�r�A�r�A�t�A�r�A�l�A�+A��A��^A��FA���A��DA�9XA�r�A�33A�G�A�33A��A�JA��A���A���A�ffA�5?A���A���A���A�;dA�%A�A�A���A�ȴA��7A�5?A���A��7A�&�A��A��A�O�A��A���A��yA���A�G�A� �A��jA��
A��uA�hsA��A�v�A��/A��wA��A��RA���A�ȴA���A�9XA�$�A���A��FA��-A�M�A�&�A�A~��A|^5AzjAx=qAwdZAvĜAu�FAs�#AsO�AsoAr�RAr1AqO�AqAp(�Ao��AohsAn��AnbNAmt�Aln�Aj��Ah=qAg��Ag"�Ag%Af�RAf�DAel�Ac�FAb�A`�RA`$�A_�7A^�uA]�A]l�A]&�A\ĜA\I�A[�A[�hA[33AZ��AX�/AU�^AUoATr�ATA�ASK�AR(�APZAMAK�TAI�#AH��AHQ�AH �AG�wAGAFz�AFJAE�wAEx�AE33AD�`ADz�ADbNACXABM�AA��AAVA?�mA?��A?�A>��A<A:�uA:�uA:�\A:bA9�A9XA9�A8��A8�HA8�RA8v�A8E�A8=qA8$�A8 �A7�;A7x�A6z�A3�;A2��A2VA0��A0�RA0r�A0-A/��A/p�A.�/A.�A,�RA*�uA)�A)�FA)p�A(��A'+A&9XA%\)A$VA$ �A#�-A"~�A!;dA �9A z�A {A��AhsAS�AG�A"�A
=A��A�`A��AoA�A��A��AO�A��A�A�A��A �AƨA�hA��AA�A��At�AI�A�Ap�A�`AjA�A%Al�A��A
z�A
A�A	��A�jAx�A7LA��AI�A�A�HA�A 5?@�dZ@�
=@���@��@�I�@�K�@��y@���@�v�@��u@��!@�z�@�K�@���@�&�@���@���@�"�@�{@��`@��;@�dZ@��@�b@�+@�hs@߾w@��y@�ff@�7L@�j@�
=@��#@ؓu@ם�@���@�~�@ա�@�X@�I�@ҟ�@��@�Q�@��@�@�J@���@˅@�~�@���@�`B@���@�Z@���@Ə\@Ų-@Ł@�G�@��@�Q�@�I�@�ƨ@�dZ@�K�@�@��h@��@��@��-@�r�@��@��;@��@��P@�t�@�
=@�-@�p�@���@��D@�ƨ@�o@��R@�n�@�-@�$�@���@�x�@�&�@��;@�-@��9@��@�;d@�~�@��#@�x�@�/@��m@�$�@�  @�\)@��+@��#@�`B@�&�@���@���@��D@��@��@�z�@�r�@�bN@�I�@�1@�^5@�G�@��/@���@�9X@��m@�l�@�ȴ@�p�@���@��@��m@���@�t�@�ff@�r�@�1@��@���@��w@��@���@��@�K�@�
=@���@�@�r�@��!@��h@�7L@��/@��@��D@���@�;d@�~�@�@��@���@���@�p�@�?}@�&�@���@�r�@��@�t�@�C�@��y@��+@��@��@��T@���@��-@��7@�O�@�?}@�&�@��`@��u@�9X@�1@��;@���@�S�@���@���@�~�@�ff@�E�@�-@�x�@���@���@���@��P@�t�@�\)@�K�@��@��@��R@���@�$�@���@�hs@�7L@�7L@�7L@��@���@�j@�;@~v�@~@}�T@}��@}O�@|(�@{�m@{�F@{�@{"�@z��@zn�@z^5@zJ@yX@x�`@x�@xb@w�P@w+@v��@v�@v��@vV@u�T@t�@s��@r-@qhs@q&�@p��@pĜ@p�@pA�@pb@o�@o�@o�P@o;d@n�R@n{@m?}@l�@l�D@lj@lI�@lI�@l(�@l�@l1@k�@j�H@j�\@i��@i&�@h�9@hQ�@g��@g�P@gl�@gK�@g;d@g
=@e��@e�@d�j@dz�@c��@c"�@b�\@a��@aX@`r�@`1'@`b@_�@_��@^��@^V@^$�@]�T@]�-@]p�@\��@\��@\�@\�D@\j@\1@[��@[dZ@[C�@["�@[@Z��@Z^5@ZM�@Z-@ZJ@Y��@Y��@Y�@Y�#@Y��@YX@X��@X��@Xr�@W��@W\)@WK�@V��@V��@V��@VV@VE�@V$�@U�T@U�-@U�h@U�h@Up�@UV@T�j@T��@Tj@TI�@T(�@T�@S��@Sƨ@S"�@R^5@R-@Q�@Q�#@Q��@Q�7@QX@Q&�@P��@P�u@PQ�@O�@O�@OK�@O
=@NE�@M�@MO�@L�@L9X@K�
@K�@KS�@K@J�\@J^5@J^5@J=q@J-@I�#@Ihs@I%@HĜ@Hr�@HQ�@H1'@G�w@F��@F5?@E@E�h@E�@E�@Ep�@E`B@E`B@E`B@EO�@E?}@E/@E/@E�@D�@D�j@D�@Dz�@Dj@DZ@D9X@D1@C�
@CdZ@Bn�@A��@A&�@?�P@>ff@=��@;�
@:�H@:J@9�@9��@9��@9�7@9�7@9x�@9hs@9X@97L@9&�@9�@9%@8��@8��@8��@8��@8�@8A�@8b@7�;@7�P@7�@5�@5�@4��@4�/@4�/@4�/@4�j@4j@4�@3�m@3�m@3�m@3�m@3�m@3��@3��@3�m@3��@3C�@3o@2�H@2�H@2��@2��@2�!@2��@2��@2M�@1�#@1�@/�;@/�w@/�P@/K�@.�y@.ȴ@.�R@.��@.v�@.$�@-�@-�T@-��@-@-�h@-O�@,��@+�m@+��@+t�@+dZ@+dZ@+S�@+S�@+33@+"�@+@*�!@)��@)��@)hs@)G�@)�@)%@)%@)%@(�`@(�u@(A�@( �@(  @'��@'��@'|�@'K�@'�@&�y@&��@&V@&$�@&{@&{@&{@&$�@&{@%�@%�T@%�T@%��@%V@$�j@$�D@$j@#ƨ@#S�@#33@"�!@"�@!x�@!G�@!G�@!7L@!&�@ ��@ �9@ �@ A�@ b@   @��@�@|�@
=@ȴ@ȴ@��@��@�+@$�@�@�T@�T@�T@�T@��@@@�-@�-@�-@�@O�@��@��@��@1@��@33@�H@��@�!@~�@^5@M�@=q@-@-@�@�@J@��@��@G�@&�@�@%@�`@�u@bN@�@�@��@�P@l�@;d@��@ff@E�@$�@��@�-@��@O�@�j@j@�@�m@�
@�F@�@�@dZ@C�@33@"�@o@��@-@�@��@�#@��@�7@X@&�@�`@Ĝ@��@�u@b@�;@�w@��@�P@|�@l�@\)@K�@;d@��@��@V@$�@@��@�-@�h@p�@�@�@�/@�D@Z@9X@��@�
@��@t�@S�@33@
�@
M�@
-@
-@
J@	�#@	%@�u@�@bN@Q�@A�@1'@b@  @�;@��@��@�w@�w@�@��@�P@��@�P@�P@�P@�P@�P@�P@�P@�P@�P@�P@��@�P@|�@\)@K�@K�@K�@;d@+@��@�@ȴ@�R@��@��@�+@ff@E�@$�@{@�@�@��@�-@�h@�@`B@V@��@�/@��@z�@z�@I�@�m@��@dZ@@��@�!@��@��@~�@~�@��@�^@�7@x�@&�@ �`@ �`@ r�@  �@ 1'@   @   @   ?��;?��;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�bNA�^5A�`BA�dZA�hsA�n�A�t�A�r�A�r�A�t�A�r�A�l�A�+A��A��^A��FA���A��DA�9XA�r�A�33A�G�A�33A��A�JA��A���A���A�ffA�5?A���A���A���A�;dA�%A�A�A���A�ȴA��7A�5?A���A��7A�&�A��A��A�O�A��A���A��yA���A�G�A� �A��jA��
A��uA�hsA��A�v�A��/A��wA��A��RA���A�ȴA���A�9XA�$�A���A��FA��-A�M�A�&�A�A~��A|^5AzjAx=qAwdZAvĜAu�FAs�#AsO�AsoAr�RAr1AqO�AqAp(�Ao��AohsAn��AnbNAmt�Aln�Aj��Ah=qAg��Ag"�Ag%Af�RAf�DAel�Ac�FAb�A`�RA`$�A_�7A^�uA]�A]l�A]&�A\ĜA\I�A[�A[�hA[33AZ��AX�/AU�^AUoATr�ATA�ASK�AR(�APZAMAK�TAI�#AH��AHQ�AH �AG�wAGAFz�AFJAE�wAEx�AE33AD�`ADz�ADbNACXABM�AA��AAVA?�mA?��A?�A>��A<A:�uA:�uA:�\A:bA9�A9XA9�A8��A8�HA8�RA8v�A8E�A8=qA8$�A8 �A7�;A7x�A6z�A3�;A2��A2VA0��A0�RA0r�A0-A/��A/p�A.�/A.�A,�RA*�uA)�A)�FA)p�A(��A'+A&9XA%\)A$VA$ �A#�-A"~�A!;dA �9A z�A {A��AhsAS�AG�A"�A
=A��A�`A��AoA�A��A��AO�A��A�A�A��A �AƨA�hA��AA�A��At�AI�A�Ap�A�`AjA�A%Al�A��A
z�A
A�A	��A�jAx�A7LA��AI�A�A�HA�A 5?@�dZ@�
=@���@��@�I�@�K�@��y@���@�v�@��u@��!@�z�@�K�@���@�&�@���@���@�"�@�{@��`@��;@�dZ@��@�b@�+@�hs@߾w@��y@�ff@�7L@�j@�
=@��#@ؓu@ם�@���@�~�@ա�@�X@�I�@ҟ�@��@�Q�@��@�@�J@���@˅@�~�@���@�`B@���@�Z@���@Ə\@Ų-@Ł@�G�@��@�Q�@�I�@�ƨ@�dZ@�K�@�@��h@��@��@��-@�r�@��@��;@��@��P@�t�@�
=@�-@�p�@���@��D@�ƨ@�o@��R@�n�@�-@�$�@���@�x�@�&�@��;@�-@��9@��@�;d@�~�@��#@�x�@�/@��m@�$�@�  @�\)@��+@��#@�`B@�&�@���@���@��D@��@��@�z�@�r�@�bN@�I�@�1@�^5@�G�@��/@���@�9X@��m@�l�@�ȴ@�p�@���@��@��m@���@�t�@�ff@�r�@�1@��@���@��w@��@���@��@�K�@�
=@���@�@�r�@��!@��h@�7L@��/@��@��D@���@�;d@�~�@�@��@���@���@�p�@�?}@�&�@���@�r�@��@�t�@�C�@��y@��+@��@��@��T@���@��-@��7@�O�@�?}@�&�@��`@��u@�9X@�1@��;@���@�S�@���@���@�~�@�ff@�E�@�-@�x�@���@���@���@��P@�t�@�\)@�K�@��@��@��R@���@�$�@���@�hs@�7L@�7L@�7L@��@���@�j@�;@~v�@~@}�T@}��@}O�@|(�@{�m@{�F@{�@{"�@z��@zn�@z^5@zJ@yX@x�`@x�@xb@w�P@w+@v��@v�@v��@vV@u�T@t�@s��@r-@qhs@q&�@p��@pĜ@p�@pA�@pb@o�@o�@o�P@o;d@n�R@n{@m?}@l�@l�D@lj@lI�@lI�@l(�@l�@l1@k�@j�H@j�\@i��@i&�@h�9@hQ�@g��@g�P@gl�@gK�@g;d@g
=@e��@e�@d�j@dz�@c��@c"�@b�\@a��@aX@`r�@`1'@`b@_�@_��@^��@^V@^$�@]�T@]�-@]p�@\��@\��@\�@\�D@\j@\1@[��@[dZ@[C�@["�@[@Z��@Z^5@ZM�@Z-@ZJ@Y��@Y��@Y�@Y�#@Y��@YX@X��@X��@Xr�@W��@W\)@WK�@V��@V��@V��@VV@VE�@V$�@U�T@U�-@U�h@U�h@Up�@UV@T�j@T��@Tj@TI�@T(�@T�@S��@Sƨ@S"�@R^5@R-@Q�@Q�#@Q��@Q�7@QX@Q&�@P��@P�u@PQ�@O�@O�@OK�@O
=@NE�@M�@MO�@L�@L9X@K�
@K�@KS�@K@J�\@J^5@J^5@J=q@J-@I�#@Ihs@I%@HĜ@Hr�@HQ�@H1'@G�w@F��@F5?@E@E�h@E�@E�@Ep�@E`B@E`B@E`B@EO�@E?}@E/@E/@E�@D�@D�j@D�@Dz�@Dj@DZ@D9X@D1@C�
@CdZ@Bn�@A��@A&�@?�P@>ff@=��@;�
@:�H@:J@9�@9��@9��@9�7@9�7@9x�@9hs@9X@97L@9&�@9�@9%@8��@8��@8��@8��@8�@8A�@8b@7�;@7�P@7�@5�@5�@4��@4�/@4�/@4�/@4�j@4j@4�@3�m@3�m@3�m@3�m@3�m@3��@3��@3�m@3��@3C�@3o@2�H@2�H@2��@2��@2�!@2��@2��@2M�@1�#@1�@/�;@/�w@/�P@/K�@.�y@.ȴ@.�R@.��@.v�@.$�@-�@-�T@-��@-@-�h@-O�@,��@+�m@+��@+t�@+dZ@+dZ@+S�@+S�@+33@+"�@+@*�!@)��@)��@)hs@)G�@)�@)%@)%@)%@(�`@(�u@(A�@( �@(  @'��@'��@'|�@'K�@'�@&�y@&��@&V@&$�@&{@&{@&{@&$�@&{@%�@%�T@%�T@%��@%V@$�j@$�D@$j@#ƨ@#S�@#33@"�!@"�@!x�@!G�@!G�@!7L@!&�@ ��@ �9@ �@ A�@ b@   @��@�@|�@
=@ȴ@ȴ@��@��@�+@$�@�@�T@�T@�T@�T@��@@@�-@�-@�-@�@O�@��@��@��@1@��@33@�H@��@�!@~�@^5@M�@=q@-@-@�@�@J@��@��@G�@&�@�@%@�`@�u@bN@�@�@��@�P@l�@;d@��@ff@E�@$�@��@�-@��@O�@�j@j@�@�m@�
@�F@�@�@dZ@C�@33@"�@o@��@-@�@��@�#@��@�7@X@&�@�`@Ĝ@��@�u@b@�;@�w@��@�P@|�@l�@\)@K�@;d@��@��@V@$�@@��@�-@�h@p�@�@�@�/@�D@Z@9X@��@�
@��@t�@S�@33@
�@
M�@
-@
-@
J@	�#@	%@�u@�@bN@Q�@A�@1'@b@  @�;@��@��@�w@�w@�@��@�P@��@�P@�P@�P@�P@�P@�P@�P@�P@�P@�P@��@�P@|�@\)@K�@K�@K�@;d@+@��@�@ȴ@�R@��@��@�+@ff@E�@$�@{@�@�@��@�-@�h@�@`B@V@��@�/@��@z�@z�@I�@�m@��@dZ@@��@�!@��@��@~�@~�@��@�^@�7@x�@&�@ �`@ �`@ r�@  �@ 1'@   @   @   ?��;?��;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B�}B��B�}B�}B�^B�?B�-B�'B�!B�B��B��B��B��B��B��B��B��B��B�{B�bB�JB�+B�B�Bz�Bw�Bw�Bv�Bu�Bq�Bn�BiyB]/BYBQ�BM�BJ�BG�BA�B9XB"�BB�HB��B��B��B��B�oB�1B�DB�Bk�BYBG�B7LB'�B{B
��B
�yB
��B
�3B
��B
��B
��B
��B
�DB
{�B
n�B
aHB
\)B
W
B
P�B
E�B
B�B
@�B
=qB
9XB
5?B
2-B
.B
,B
(�B
%�B
"�B
�B
�B
PB
  B	��B	��B	��B	��B	�B	�B	�HB	�B	��B	��B	ǮB	B	�wB	�dB	�XB	�LB	�9B	�-B	�B	�B	��B	��B	�JB	�1B	�B	�B	{�B	t�B	iyB	\)B	R�B	H�B	B�B	A�B	?}B	=qB	:^B	7LB	5?B	49B	33B	1'B	0!B	.B	-B	'�B	"�B	�B	�B	�B	�B	uB	VB	B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�yB�ZB�B�B��B��B��B��B��BɺBƨBÖB��B�dB�FB�9B�3B�'B�B�B��B��B��B��B��B��B��B��B��B�{B�uB�oB�oB�oB�hB�hB�bB�\B�PB�=B�+B�%B�%B�B�B� B}�B{�Bz�By�Bx�Bv�Bt�Bs�Bs�Bq�Bn�BjBgmBe`BdZBbNB_;B[#BXBT�BS�BR�BO�BN�BL�BK�BJ�BH�BE�BA�B@�B>wB>wB=qB<jB<jB;dB;dB:^B9XB8RB6FB5?B49B49B33B1'B2-B1'B1'B0!B0!B/B.B/B-B.B/B/B.B.B.B/B/B0!B0!B0!B0!B0!B0!B0!B0!B1'B33B2-B33B33B49B5?B6FB6FB7LB7LB7LB7LB9XB:^B:^B:^B:^B:^B:^B:^B:^B:^B9XB;dB<jB=qB?}BA�BA�BA�BA�BA�BA�BA�BB�BC�BD�BE�BF�BG�BG�BH�BH�BH�BH�BH�BH�BJ�BN�BR�BT�BVBW
BYBYBXB[#B_;BdZBffBhsBiyBjBk�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bq�Bt�Bu�Bu�Bw�Bw�Bx�Bz�B�B�DB�VB�\B�VB�\B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�}BĜBƨBȴBɺB��B��B��B�B�#B�)B�/B�5B�;B�HB�HB�NB�`B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	%B	+B		7B	DB	VB	bB	hB	oB	uB	uB	�B	�B	$�B	'�B	(�B	(�B	)�B	)�B	,B	-B	.B	.B	2-B	5?B	8RB	9XB	9XB	9XB	:^B	<jB	?}B	C�B	I�B	K�B	K�B	L�B	M�B	R�B	S�B	T�B	T�B	W
B	XB	ZB	ZB	\)B	^5B	`BB	aHB	cTB	ffB	gmB	hsB	hsB	iyB	jB	l�B	p�B	u�B	z�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�PB	�\B	�\B	�\B	�bB	�bB	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�?B	�FB	�^B	�dB	�}B	��B	��B	��B	B	ĜB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
+B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
JB
JB
JB
JB
JB
JB
PB
\B
bB
hB
{B
�B
�B
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
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
$�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
(�B
)�B
)�B
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
-B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
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
?}B
?}B
?}B
?}B
@�B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
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
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
O�B
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
Q�B
R�B
R�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
XB
XB
XB
XB
YB
YB
YB
ZB
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
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
_;B
`BB
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
cTB
cTB
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
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
jB
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
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
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
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B� B��B��B�GB�vB��B��B�8B�B��B��B��B��B��B�B��B��B��B��B��B��B��B{JBw�Bw�Bv�BvFBraBoiBjB]�BY�BR�BN�BKDBHfBB�B;�B%�B�B�B�\B� B�aB�qB�B�7B��B�{Bm�B[�BJ=B9�B*�B�B
��B
�B
�B
�tB
��B
�IB
��B
�YB
��B
~BB
p�B
bhB
]/B
X�B
R�B
FYB
CB
A B
>]B
:DB
5�B
3B
.�B
,�B
)�B
&�B
$&B
OB
�B
�B
 �B	�jB	�$B	�RB	�zB	�ZB	�B	�:B	ٚB	бB	̳B	��B	�aB	�B	��B	��B	��B	��B	��B	��B	�/B	��B	��B	�6B	�B	��B	�{B	}�B	wLB	l�B	^�B	U2B	J	B	B�B	A�B	@B	>]B	;0B	7�B	5�B	4�B	3�B	1�B	0�B	.�B	.cB	)DB	#�B	 �B	�B	�B	YB	2B	B	�B�(B�BB��B�B�$B�8B��B�B�B�B��B��B��B��B�5B�wB�6B�B�qB�?B�aB�VB�6B�JB�^B�XBǔBĜB�B��B�B��B��B�|B��B�WB�>B�B�nB��B�vB�B�KB�$B�B�B��B��B��B��B��B��B��B�HB�(B�xB��B��B��B��B�gB�;B~�B|�B{BzxBy�Bw�Bu?BtBtnBsMBpUBlqBh>Bf2Be`BdBabB]/BY�BU�BT�BTFBQNBO\BM�BL~BK�BJ�BH�BD�BA;B>�B>�B>BB=qB=B;�B;�B;B:�B9�B7�B6+B5ZB4�B4nB3B3hB2B1�B0�B0�B/�B/�B0B.IB/OB/�B/�B/B.�B/B0B0B0�B0�B0�B0�B0�B1B1vB1'B1�B3�B3B4B49B5%B6B6�B6�B7�B7�B7�B88B9�B:�B:�B:�B:�B:�B:�B:�B:�B:�B:^B<PB=qB>wB@OBA�BA�BA�BA�BA�BBBBABC-BC�BEBF?BG+BG�BG�BIBH�BIBIBI7BI�BK�BO�BS�BU�BV�BW�BY�BY�BYKB\�B`�Bd�BgBiBi�Bj�Bk�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bn/Bn�BraBuBvBv+Bx8BxRByrB{�B��B��B��B��B��B�bB��B��B��B�B��B�B�B�,B�2B�8B�sB��B�CB�tB�4B��B��B�B�	B�DB�pB�uB�_B�=B�]B�dB�jB�pB�|B�B��B��B�B��B��B��B�B��B��B��B�	B�B�B�B�"B�VB�cB	[B	SB	YB	zB		�B	�B	�B	}B	�B	�B	�B	B	1B	 BB	%,B	(
B	)B	)B	*B	*0B	,=B	-CB	.IB	.}B	2|B	5�B	8lB	9�B	9rB	9�B	:�B	<�B	?�B	D3B	I�B	K�B	K�B	MB	N<B	SB	TB	UB	U2B	W?B	X_B	Z7B	ZQB	\�B	^jB	`vB	a|B	c�B	f�B	g�B	h�B	h�B	i�B	j�B	mB	qB	vFB	{0B	B	�B	�B	�;B	�'B	�-B	�-B	�MB	�9B	�SB	�YB	��B	��B	��B	��B	�vB	�vB	�bB	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�8B	��B	�]B	�OB	�UB	�vB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	� B	� B	� B	�&B	�B	�,B	�,B	�B	�B	�B	�2B	�9B	�?B	�+B	�1B	�eB	�qB	�IB	�IB	�OB	�VB	�pB	�\B	�bB	�bB	�hB	�hB	�TB	�nB	�B	�tB	�zB	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�8B	�$B	�B	�B	�"B	�B	�(B	�.B
 B
  B
 B
 B
 4B
;B
;B
'B
'B
-B
-B
aB
�B
zB
fB
	RB
	7B
	7B
	7B
	RB

=B

=B

=B

=B

=B

=B

XB

XB
^B
^B
^B
JB
dB
~B
~B
~B
~B
�B
�B
�B
 B
�B
�B
EB
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
 �B
 �B
 �B
!�B
!�B
!�B
!�B
#B
#:B
%,B
'B
'B
&�B
&�B
'B
'B
($B
)B
)B
(�B
(�B
)�B
)�B
)�B
)B
*B
*0B
*B
+B
+B
+B
+B
+B
+B
+6B
+6B
,=B
,WB
-�B
/5B
/5B
0;B
0UB
1AB
1AB
1AB
1AB
2GB
2GB
33B
3MB
3MB
3MB
3hB
4�B
4�B
6`B
7fB
7LB
7fB
7LB
7LB
7fB
7�B
7fB
7�B
8�B
9rB
:xB
:xB
;�B
;B
;dB
;dB
;B
;�B
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
?�B
?�B
?�B
?}B
@�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
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
L�B
L�B
L�B
L�B
NB
NB
NB
O(B
O�B
Q B
QB
Q B
RB
RB
Q�B
Q�B
Q�B
Q�B
RB
Q�B
SB
SB
RB
SB
SB
R�B
SB
SB
S&B
TB
T,B
U2B
T�B
U2B
UB
U2B
V9B
W$B
X+B
X+B
X+B
X+B
Y1B
YKB
YKB
Z7B
[=B
[=B
[#B
[=B
[=B
\)B
\CB
\CB
\)B
\CB
\CB
\]B
]dB
^5B
^OB
^OB
^OB
^OB
^jB
_VB
_VB
_VB
_VB
`\B
_pB
`\B
abB
a|B
aHB
aHB
aHB
abB
abB
bhB
bhB
b�B
cnB
cnB
cnB
c�B
dtB
d�B
dtB
dtB
ezB
ezB
ezB
ezB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
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
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
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
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802280034222018022800342220180228003422202211182133422022111821334220221118213342201804031939232018040319392320180403193923  JA  ARFMdecpA19c                                                                20180218003531  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180217153611  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180217153612  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180217153613  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180217153613  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180217153613  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180217153614  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180217153614  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180217153614  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180217153614                      G�O�G�O�G�O�                JA  ARUP                                                                        20180217155626                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180217153149  CV  JULD            G�O�G�O�F�h�                JM  ARGQJMQC2.0                                                                 20180217153149  CV  JULD_LOCATION   G�O�G�O�F�i                JM  ARGQJMQC2.0                                                                 20180217153149  CV  LONGITUDE       G�O�G�O��!��                JM  ARCAJMQC2.0                                                                 20180227153422  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180227153422  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103923  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171526                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123342  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                
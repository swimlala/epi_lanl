CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-05-01T01:17:49Z creation; 2022-04-26T16:06:57Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     8  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  Z@   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     8  a�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  ~�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     8  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  �P   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8 	�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P &�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8 .    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P KX   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8 R�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` o�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   p@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   v@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   |@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210501011749  20220426232404  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_017                 8138_008904_017                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @�q>f�Q@�q>f�Q11  @�q>8�YK@�q>8�YK@-uH���L@-uH���L�dN���1f�dN���1f11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@=p�@�  @�G�@�G�@�G�A ��A  A ��A,��A@  A`  A\)A��A�  A�  A��AϮA߮A�A��B  B  B  B (�B(  B0(�B8(�B@(�BH(�BP  BX(�B`(�Bh  Bo�
Bx  B�
B�  B�  B�  B�{B�ffB�  B��B��B��B�{B�  B�  B�  B�{B�{B�{B�{B��B�  B�  B�  B��B�  B�  B�  B�  B�{B�  B��B��B��C 
=C
=C  C��C��C	��C��C  C
=C
=C
=C
=C  C��C��C  C   C"  C$  C&  C(  C)��C,  C.
=C0
=C2
=C4
=C6
=C8
=C:  C<
=C>{C@{CB  CD  CF  CG��CJ  CL  CN  CP
=CR  CS��CV  CX  CY��C[��C^  C`  Ca��Cd
=Cf
=Ch  Cj
=Cl
=Cn
=Cp
=Cr
=Ct  Cv  Cw��Cz  C|
=C~
=C�C�C�  C���C���C�  C�  C���C���C���C�  C�  C�  C�  C���C�  C�C�C���C�  C�  C�  C�C�  C���C���C�  C�C�  C�C�C�C�  C���C�  C�C�  C���C�  C�C�  C�  C�  C���C���C���C�  C�  C�  C�  C�  C���C�C�C���C�  C�C�  C�  C�  C���C�  C�  C�C�  C���C�  C�C�C�  C�  C�C�C�  C���C���C�  C�C�C���C���C�  C�  C�  C���C���C�  C�  C�C�  C���C�  C���C���C�  C�C�C�C�C�C�C�  C���C�C�  C�  C���C���C�  C�C�  C�  C���C���C�  C�C�  C���C���C�  C�  C���C���C���C�  C�  C�  C�  D �D ��DD��D�qD� D  D� D�qD� D�qD� D  D}qD�qD� D�D��D	  D	}qD
  D
��D  D}qD��D� D�D��D  D}qD��D}qD  D}qD�qD}qD�qD}qD�qD� D  D� D�D��D  D}qD  D� D�qD� D�D��D�D��D�qD}qD�qD� D�D� D�qD� D�D}qD   D ��D!  D!}qD!��D"}qD#�D#� D#�qD$� D%  D%� D&  D&z�D&��D'z�D'�qD(� D)�D)�D*  D*z�D*�qD+� D,  D,��D-D-� D-�qD.}qD/  D/��D/�qD0z�D0��D1z�D1�qD2}qD3  D3��D3�qD4z�D4�qD5� D5�qD6}qD7  D7�D8D8��D9�D9� D:  D:}qD;�D;��D;�qD<}qD=  D=��D>�D>� D?  D?}qD?�qD@}qDA  DA}qDA�qDB��DC  DC� DD  DD��DE�DE}qDE�qDF� DG  DG� DH  DH� DI  DI}qDI�qDJ� DK�DK� DK�qDL� DL�qDM}qDM�qDN� DO  DO� DP  DP� DQ  DQ��DR  DR}qDR�qDS� DS�qDT}qDT�qDU� DU�qDV}qDW  DW��DX  DX}qDY  DY}qDY�qDZ� D[  D[}qD[�qD\� D]  D]}qD^  D^��D_  D_}qD`  D`��Da�Da� Db  Db� Db�qDc}qDd  Dd� De�De��Df�Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj��Dk�Dk� Dl  Dl� Dm�Dm��Dn  Dn}qDo  Do��DpDp��Dq�Dq��Dr�Dr� Dr��Ds}qDt  Dt��Du  Du}qDv  Dv��Dw  Dw}qDw�qDx� Dy  Dy��Dz�Dz��D{�D{��D|�D|��D|�qD}� D~  D~� D~�qD� D�qD�=qD�� D��HD�HD�@ D�~�D���D��qD�@ D���D��HD�  D�AHD�~�D��qD���D�>�D�~�D�� D�HD�AHD���D��HD�HD�@ D�� D�� D���D�>�D�~�D��HD�  D�@ D�� D���D�HD�@ D�}qD�� D�  D�>�D�� D��HD�HD�@ D��HD��HD�HD�@ D��HD�� D�HD�@ D�~�D���D���D�@ D�~�D���D���D�@ D�� D��HD�HD�@ D��HD��HD���D�>�D�~�D���D�  D�AHD��HD��HD�HD�@ D�}qD�� D��D�@ D�� D��HD�HD�>�D�� D��HD�HD�AHD�� D���D�  D�AHD�� D�� D���D�@ D��HD�� D�  D�>�D�� D��HD�HD�@ D�}qD�� D�HD�AHD��HD���D�  D�AHD�� D���D�HD�AHD�� D���D�  D�AHD�� D�� D�  D�@ D�� D���D���D�@ D��HD�� D�  D�>�D�� D��HD�HD�AHD��HD�� D�  D�AHD��HD�� D�  D�AHD�~�D�D�HD�>�D�~�D�� D���D�@ D�~�D�� D�  D�AHD�� D�� D���D�>�D�~�D���D���D�@ D��HD��HD�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�� D��HD�  D�@ D�~�D���D���D�>�D�� D�� D���D�@ D��HD��HD�  D�@ D�~�D��HD�HD�@ D�� D��HD�  D�>�D�� D��HD�HD�AHD��HD��HD�HD�AHD�� D���D�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�AHD�� D���D���D�@ D�� D��qD���D�>�D�~�D�� D�  D�@ D�~�D�� D�  D�@ D��HD�� D���D�>�D�� D��HD�  D�>�D�� D�� D�  D�AHD�� D��HD�HD�>�D�~�D�� D�  D�AHD�~�DýqD���D�AHDĀ Dľ�D���D�>�D�~�Dž�D�  D�AHDƀ D�� D�  D�AHDǀ D�� D�HD�AHDȁHD�� D���D�@ Dɀ D�� D�  D�>�D�~�DʽqD���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�@ D̀ D�� D�  D�>�D�~�DνqD�  D�@ D�~�DϾ�D���D�@ DЁHD�� D�  D�>�Dр D�� D�  D�@ DҀ D�� D���D�>�D�~�DӾ�D���D�>�D�~�D�� D�HD�@ D�~�Dվ�D���D�@ Dր D��HD�HD�>�D׀ D׾�D���D�>�D؀ D�� D�  D�@ Dـ D��HD���D�@ Dڀ D�� D�HD�AHDہHD�� D�  D�AHD܀ D�� D�  D�@ D݀ D�� D���D�@ DށHD�� D�  D�@ D߀ D�� D�HD�AHD�� DྸD�  D�@ D�HD��HD�  D�@ D�~�D⾸D�  D�@ D� D㾸D�  D�@ D� D�� D�  D�@ D�HD��HD�HD�>�D�}qD�qD��qD�>�D�}qD��HD�HD�@ ?�?\)?W
=?�=q?���?��?��@�@(�@.{@=p�@Q�@c�
@p��@��
@��@�33@�p�@�ff@�\)@�
=@�G�@�=q@�33@��H@��@�\)@�
=A ��A�A��Ap�A�\A
=A�A ��A%�A*=qA.�RA3�
A7�A<(�A@��AEAI��AN�RAS33AW�A\(�A`��Adz�Ah��Amp�Aq�AuAz�HA\)A��A�z�A�
=A�G�A��A�ffA���A�33A�{A�Q�A��\A�p�A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ?�  @�\@=p�@�  @�G�@�G�@�G�A ��A  A ��A,��A@  A`  A\)A��A�  A�  A��AϮA߮A�A��B  B  B  B (�B(  B0(�B8(�B@(�BH(�BP  BX(�B`(�Bh  Bo�
Bx  B�
B�  B�  B�  B�{B�ffB�  B��B��B��B�{B�  B�  B�  B�{B�{B�{B�{B��B�  B�  B�  B��B�  B�  B�  B�  B�{B�  B��B��B��C 
=C
=C  C��C��C	��C��C  C
=C
=C
=C
=C  C��C��C  C   C"  C$  C&  C(  C)��C,  C.
=C0
=C2
=C4
=C6
=C8
=C:  C<
=C>{C@{CB  CD  CF  CG��CJ  CL  CN  CP
=CR  CS��CV  CX  CY��C[��C^  C`  Ca��Cd
=Cf
=Ch  Cj
=Cl
=Cn
=Cp
=Cr
=Ct  Cv  Cw��Cz  C|
=C~
=C�C�C�  C���C���C�  C�  C���C���C���C�  C�  C�  C�  C���C�  C�C�C���C�  C�  C�  C�C�  C���C���C�  C�C�  C�C�C�C�  C���C�  C�C�  C���C�  C�C�  C�  C�  C���C���C���C�  C�  C�  C�  C�  C���C�C�C���C�  C�C�  C�  C�  C���C�  C�  C�C�  C���C�  C�C�C�  C�  C�C�C�  C���C���C�  C�C�C���C���C�  C�  C�  C���C���C�  C�  C�C�  C���C�  C���C���C�  C�C�C�C�C�C�C�  C���C�C�  C�  C���C���C�  C�C�  C�  C���C���C�  C�C�  C���C���C�  C�  C���C���C���C�  C�  C�  C�  D �D ��DD��D�qD� D  D� D�qD� D�qD� D  D}qD�qD� D�D��D	  D	}qD
  D
��D  D}qD��D� D�D��D  D}qD��D}qD  D}qD�qD}qD�qD}qD�qD� D  D� D�D��D  D}qD  D� D�qD� D�D��D�D��D�qD}qD�qD� D�D� D�qD� D�D}qD   D ��D!  D!}qD!��D"}qD#�D#� D#�qD$� D%  D%� D&  D&z�D&��D'z�D'�qD(� D)�D)�D*  D*z�D*�qD+� D,  D,��D-D-� D-�qD.}qD/  D/��D/�qD0z�D0��D1z�D1�qD2}qD3  D3��D3�qD4z�D4�qD5� D5�qD6}qD7  D7�D8D8��D9�D9� D:  D:}qD;�D;��D;�qD<}qD=  D=��D>�D>� D?  D?}qD?�qD@}qDA  DA}qDA�qDB��DC  DC� DD  DD��DE�DE}qDE�qDF� DG  DG� DH  DH� DI  DI}qDI�qDJ� DK�DK� DK�qDL� DL�qDM}qDM�qDN� DO  DO� DP  DP� DQ  DQ��DR  DR}qDR�qDS� DS�qDT}qDT�qDU� DU�qDV}qDW  DW��DX  DX}qDY  DY}qDY�qDZ� D[  D[}qD[�qD\� D]  D]}qD^  D^��D_  D_}qD`  D`��Da�Da� Db  Db� Db�qDc}qDd  Dd� De�De��Df�Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj��Dk�Dk� Dl  Dl� Dm�Dm��Dn  Dn}qDo  Do��DpDp��Dq�Dq��Dr�Dr� Dr��Ds}qDt  Dt��Du  Du}qDv  Dv��Dw  Dw}qDw�qDx� Dy  Dy��Dz�Dz��D{�D{��D|�D|��D|�qD}� D~  D~� D~�qD� D�qD�=qD�� D��HD�HD�@ D�~�D���D��qD�@ D���D��HD�  D�AHD�~�D��qD���D�>�D�~�D�� D�HD�AHD���D��HD�HD�@ D�� D�� D���D�>�D�~�D��HD�  D�@ D�� D���D�HD�@ D�}qD�� D�  D�>�D�� D��HD�HD�@ D��HD��HD�HD�@ D��HD�� D�HD�@ D�~�D���D���D�@ D�~�D���D���D�@ D�� D��HD�HD�@ D��HD��HD���D�>�D�~�D���D�  D�AHD��HD��HD�HD�@ D�}qD�� D��D�@ D�� D��HD�HD�>�D�� D��HD�HD�AHD�� D���D�  D�AHD�� D�� D���D�@ D��HD�� D�  D�>�D�� D��HD�HD�@ D�}qD�� D�HD�AHD��HD���D�  D�AHD�� D���D�HD�AHD�� D���D�  D�AHD�� D�� D�  D�@ D�� D���D���D�@ D��HD�� D�  D�>�D�� D��HD�HD�AHD��HD�� D�  D�AHD��HD�� D�  D�AHD�~�D�D�HD�>�D�~�D�� D���D�@ D�~�D�� D�  D�AHD�� D�� D���D�>�D�~�D���D���D�@ D��HD��HD�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�� D��HD�  D�@ D�~�D���D���D�>�D�� D�� D���D�@ D��HD��HD�  D�@ D�~�D��HD�HD�@ D�� D��HD�  D�>�D�� D��HD�HD�AHD��HD��HD�HD�AHD�� D���D�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�AHD�� D���D���D�@ D�� D��qD���D�>�D�~�D�� D�  D�@ D�~�D�� D�  D�@ D��HD�� D���D�>�D�� D��HD�  D�>�D�� D�� D�  D�AHD�� D��HD�HD�>�D�~�D�� D�  D�AHD�~�DýqD���D�AHDĀ Dľ�D���D�>�D�~�Dž�D�  D�AHDƀ D�� D�  D�AHDǀ D�� D�HD�AHDȁHD�� D���D�@ Dɀ D�� D�  D�>�D�~�DʽqD���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�@ D̀ D�� D�  D�>�D�~�DνqD�  D�@ D�~�DϾ�D���D�@ DЁHD�� D�  D�>�Dр D�� D�  D�@ DҀ D�� D���D�>�D�~�DӾ�D���D�>�D�~�D�� D�HD�@ D�~�Dվ�D���D�@ Dր D��HD�HD�>�D׀ D׾�D���D�>�D؀ D�� D�  D�@ Dـ D��HD���D�@ Dڀ D�� D�HD�AHDہHD�� D�  D�AHD܀ D�� D�  D�@ D݀ D�� D���D�@ DށHD�� D�  D�@ D߀ D�� D�HD�AHD�� DྸD�  D�@ D�HD��HD�  D�@ D�~�D⾸D�  D�@ D� D㾸D�  D�@ D� D�� D�  D�@ D�HD��HD�HD�>�D�}qD�qD��qD�>�D�}qD��HD�HG�O�?�?\)?W
=?�=q?���?��?��@�@(�@.{@=p�@Q�@c�
@p��@��
@��@�33@�p�@�ff@�\)@�
=@�G�@�=q@�33@��H@��@�\)@�
=A ��A�A��Ap�A�\A
=A�A ��A%�A*=qA.�RA3�
A7�A<(�A@��AEAI��AN�RAS33AW�A\(�A`��Adz�Ah��Amp�Aq�AuAz�HA\)A��A�z�A�
=A�G�A��A�ffA���A�33A�{A�Q�A��\A�p�A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A˗�A˟�AˮA˟�A˓uAˍPA�|�A�v�A�x�A�v�A�n�A�bNA�S�A�O�A�I�A�A�A�?}A�9XA�5?A�5?A�5?A�1'A�+A��A���A��yA���Aʧ�AʑhAʼjAʼjA���Aʲ-AʶFAʩ�Aʙ�Aʙ�A�|�A�Q�A�/A�A�/A�`BA�XA��yA��A���A��A�ƨA���A���A�{A���Að!A�E�A�ĜA�\)A��;A�K�A�+A�dZA�A�A���A��/A��A���A��PA���A�A��A��A�
=A�bNA���A�jA���A�E�A�z�A�$�A���A�&�A�M�A���A��A�ffA�p�A���A�O�A�1'A�=qA�1'A��/A�A��A���A�M�A�A�5?A��
A���A�-A��`A�9XA���A�bNA��7A���A���A�^A|�A|�Axv�Au�7As��Ap�HAl5?Aj �Ag��Ac�TA]x�A\1AY��AU�AT�uARffAK��AEAB��ABJABz�A@�\A>��A>�uA>-A=A9�;A6 �A4(�A3XA2��A1��A1%A1\)A1��A0�A0VA0��A1\)A1O�A1��A1��A1�A1\)A0��A0�uA09XA/�FA.�uA,�!A+A+%A*I�A)��A)\)A)C�A)�A(�!A((�A'x�A%�A%��A$��A$��A#ƨA#;dA"�RA"Q�A"{A!�mA!�^A!��A!`BA!oA �A 1'A�
At�A�AJA^5A�A�7AK�A�HA�TA��AZA�^AoA%A�A��A9XA�TA��A�FA�A��A�wA��AK�A��A�\A��A�uAn�AbAƨA��A1AJA�A"�A(�A��AAl�A;dA�`A�!Av�AA�FAhsA|�At�A�A�A�A�A
�`A
��A
��A
n�A	ƨA	hsA	|�A	K�A��A�mA+Ar�A �A �A�mA�-A�7A+AĜAbNA�A�mA�wAhsA��A1'A�A��At�A`BA7LA%A ĜA �DA �@���@�"�@�v�@��T@���@��@�hs@�hs@�`B@��D@��@��y@�v�@��@��-@��@�X@��@��
@�C�@��!@��h@�Ĝ@�@�Z@�@���@�v�@�J@��@�hs@�/@��@���@�9X@���@@�J@���@�X@�z�@�K�@�@��H@�ȴ@�n�@���@��`@�I�@�w@�"�@�!@��@�V@䛦@�A�@�P@��y@�J@���@��@�j@�bN@��@�|�@ާ�@�{@�&�@���@ܼj@��@�ƨ@۝�@�C�@��y@�~�@�@ى7@���@�Z@�1@ׅ@���@ָR@֟�@�ff@�V@�J@�O�@�bN@���@ӕ�@�o@ҏ\@�M�@ѡ�@���@��@�t�@�ȴ@�E�@��@���@�x�@�%@�Z@��
@�S�@��@���@ʏ\@�V@���@ɺ^@�hs@���@ț�@�1@Ǯ@���@�M�@��@��@�@�p�@��@ě�@� �@�t�@°!@�@�p�@�?}@�&�@���@��j@� �@��
@��F@���@�C�@���@�J@���@�p�@�?}@���@��D@��m@�t�@�+@�V@���@��7@�X@��`@�bN@�(�@�K�@�J@���@��7@�p�@�/@��/@�Z@��
@�\)@�o@���@��+@��@��^@��@�O�@�&�@���@��@�Q�@�b@��@�\)@�;d@��H@�-@�@���@���@��@��j@��D@�Q�@���@��F@�t�@�+@��y@���@�n�@��@��#@�`B@�%@���@�b@��
@��@�"�@��!@�5?@�O�@�7L@�%@�Ĝ@�z�@�Z@��@��m@��m@�b@�b@��;@���@�C�@�
=@�ȴ@�ff@�J@��#@�G�@�V@��/@�j@�9X@�  @��@�"�@���@�ȴ@�M�@�{@��@�?}@���@��@��9@���@�Q�@�9X@�b@�ƨ@���@�|�@�@�~�@�-@��-@�hs@��@��j@��D@��D@�r�@�1@�l�@��H@�ff@��@�@���@��7@�p�@��@�j@�b@�t�@�33@�o@�
=@�
=@�
=@��@��\@�E�@�E�@�=q@�-@���@�@�`B@��@��@�1'@��@���@�S�@��R@���@�~�@�5?@��#@�&�@��D@�bN@��@��@�@��R@�~�@��T@���@���@��7@��@��@���@��u@�z�@�1'@���@���@�l�@�K�@�"�@�o@��@��@�~�@�n�@�ff@�V@�=q@�J@���@���@��@��j@��u@�A�@��@��
@���@�C�@��@���@��+@�V@�-@�$�@��@��-@�x�@�X@�&�@��`@���@�j@�b@�P@~��@~�@~E�@}�-@}/@}�@|�@|Z@{��@z�@z=q@z-@z�@y��@x��@xQ�@w��@w�P@v�y@vE�@up�@t�@t��@s�
@sdZ@s"�@r�@q��@qx�@p�`@o�w@o|�@n��@n�@n��@nV@m��@m?}@m/@l�/@l9X@k�@kS�@kC�@kC�@ko@j��@i�^@i%@h�@h �@g��@g�P@gK�@fv�@e@e?}@eV@d�D@c��@b��@b�\@bn�@bM�@b=q@b-@b�@bJ@a��@a�^@aX@`Q�@_\)@_�@^ȴ@^��@^�+@^ff@^{@]@]�@]/@\�@\�@Zn�@Z=q@Y��@Yhs@X�`@X��@XbN@W�;@W�@V�+@VV@U�T@U@U�-@U`B@UV@T��@T�j@Tz�@T(�@S��@Sƨ@S��@R�H@R^5@Q�#@Qhs@Q&�@P��@P �@O��@Ol�@Nȴ@Nv�@Nff@N$�@N{@M�@MO�@L�/@L�D@L�@K��@K��@K�@Kt�@J�H@J~�@J-@JJ@JJ@Ix�@H�@H �@G�@G�@G\)@Fȴ@FV@F@E�@E�@D�@Dz�@DZ@D(�@C�
@C�@CS�@Co@B��@BM�@A�#@A7L@@��@@r�@@bN@@bN@@Q�@@ �@?�P@?;d@>��@>�+@>5?@>@=��@=�-@=?}@<�/@<�j@<�j@<Z@;��@;t�@;"�@:��@:�\@:�@9�@9��@9��@9x�@9G�@97L@9%@8�`@8A�@8b@7�;@7\)@7
=@6��@6E�@5�T@5�-@5�@5`B@5/@4�@4j@4(�@3ƨ@3C�@3o@2�@2�!@2��@2-@1��@1G�@0Ĝ@0A�@/�;@/��@/\)@/
=@.�+@.5?@.{@-p�@-O�@-/@,�/@,�@,z�@,Z@,I�@,9X@+ƨ@+t�@+dZ@+C�@+"�@+o@*��@*�\@*n�@*=q@*�@)��@)7L@(��@(�`@(�@(bN@(A�@'�@'��@'�@'��@'|�@&�R@&{@%��@%��@%p�@%/@$��@$�j@$�@$�D@$z�@$(�@#��@#�m@#��@#t�@#dZ@#C�@"��@"�!@"��@"�\@"~�@!�@!x�@!&�@!�@!�@ ��@ �@ bN@ Q�@  �@�@��@�w@�w@�w@��@|�@l�@+@��@��@�RA˓uA˕�A˓uAˏ\A˟�A˟�A˕�A˝�A˴9AˬAˬAˮA˩�A˓uAˡ�A˧�AˑhA˕�A˕�AˑhAˏ\AˑhAˉ7AˋDA�t�A�|�A�x�A�v�A�r�A�x�A�t�A�v�A�z�A�v�A�v�A�z�A�v�A�n�A�p�A�n�A�hsA�`BA�\)A�`BA�`BA�\)A�XA�VA�ZA�VA�Q�A�O�A�O�A�K�A�M�A�O�A�O�A�Q�A�S�A�VA�O�A�O�A�S�A�A�A�E�A�E�A�A�A�A�A�C�A�?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   A˗�A˟�AˮA˟�A˓uAˍPA�|�A�v�A�x�A�v�A�n�A�bNA�S�A�O�A�I�A�A�A�?}A�9XA�5?A�5?A�5?A�1'A�+A��A���A��yA���Aʧ�AʑhAʼjAʼjA���Aʲ-AʶFAʩ�Aʙ�Aʙ�A�|�A�Q�A�/A�A�/A�`BA�XA��yA��A���A��A�ƨA���A���A�{A���Að!A�E�A�ĜA�\)A��;A�K�A�+A�dZA�A�A���A��/A��A���A��PA���A�A��A��A�
=A�bNA���A�jA���A�E�A�z�A�$�A���A�&�A�M�A���A��A�ffA�p�A���A�O�A�1'A�=qA�1'A��/A�A��A���A�M�A�A�5?A��
A���A�-A��`A�9XA���A�bNA��7A���A���A�^A|�A|�Axv�Au�7As��Ap�HAl5?Aj �Ag��Ac�TA]x�A\1AY��AU�AT�uARffAK��AEAB��ABJABz�A@�\A>��A>�uA>-A=A9�;A6 �A4(�A3XA2��A1��A1%A1\)A1��A0�A0VA0��A1\)A1O�A1��A1��A1�A1\)A0��A0�uA09XA/�FA.�uA,�!A+A+%A*I�A)��A)\)A)C�A)�A(�!A((�A'x�A%�A%��A$��A$��A#ƨA#;dA"�RA"Q�A"{A!�mA!�^A!��A!`BA!oA �A 1'A�
At�A�AJA^5A�A�7AK�A�HA�TA��AZA�^AoA%A�A��A9XA�TA��A�FA�A��A�wA��AK�A��A�\A��A�uAn�AbAƨA��A1AJA�A"�A(�A��AAl�A;dA�`A�!Av�AA�FAhsA|�At�A�A�A�A�A
�`A
��A
��A
n�A	ƨA	hsA	|�A	K�A��A�mA+Ar�A �A �A�mA�-A�7A+AĜAbNA�A�mA�wAhsA��A1'A�A��At�A`BA7LA%A ĜA �DA �@���@�"�@�v�@��T@���@��@�hs@�hs@�`B@��D@��@��y@�v�@��@��-@��@�X@��@��
@�C�@��!@��h@�Ĝ@�@�Z@�@���@�v�@�J@��@�hs@�/@��@���@�9X@���@@�J@���@�X@�z�@�K�@�@��H@�ȴ@�n�@���@��`@�I�@�w@�"�@�!@��@�V@䛦@�A�@�P@��y@�J@���@��@�j@�bN@��@�|�@ާ�@�{@�&�@���@ܼj@��@�ƨ@۝�@�C�@��y@�~�@�@ى7@���@�Z@�1@ׅ@���@ָR@֟�@�ff@�V@�J@�O�@�bN@���@ӕ�@�o@ҏ\@�M�@ѡ�@���@��@�t�@�ȴ@�E�@��@���@�x�@�%@�Z@��
@�S�@��@���@ʏ\@�V@���@ɺ^@�hs@���@ț�@�1@Ǯ@���@�M�@��@��@�@�p�@��@ě�@� �@�t�@°!@�@�p�@�?}@�&�@���@��j@� �@��
@��F@���@�C�@���@�J@���@�p�@�?}@���@��D@��m@�t�@�+@�V@���@��7@�X@��`@�bN@�(�@�K�@�J@���@��7@�p�@�/@��/@�Z@��
@�\)@�o@���@��+@��@��^@��@�O�@�&�@���@��@�Q�@�b@��@�\)@�;d@��H@�-@�@���@���@��@��j@��D@�Q�@���@��F@�t�@�+@��y@���@�n�@��@��#@�`B@�%@���@�b@��
@��@�"�@��!@�5?@�O�@�7L@�%@�Ĝ@�z�@�Z@��@��m@��m@�b@�b@��;@���@�C�@�
=@�ȴ@�ff@�J@��#@�G�@�V@��/@�j@�9X@�  @��@�"�@���@�ȴ@�M�@�{@��@�?}@���@��@��9@���@�Q�@�9X@�b@�ƨ@���@�|�@�@�~�@�-@��-@�hs@��@��j@��D@��D@�r�@�1@�l�@��H@�ff@��@�@���@��7@�p�@��@�j@�b@�t�@�33@�o@�
=@�
=@�
=@��@��\@�E�@�E�@�=q@�-@���@�@�`B@��@��@�1'@��@���@�S�@��R@���@�~�@�5?@��#@�&�@��D@�bN@��@��@�@��R@�~�@��T@���@���@��7@��@��@���@��u@�z�@�1'@���@���@�l�@�K�@�"�@�o@��@��@�~�@�n�@�ff@�V@�=q@�J@���@���@��@��j@��u@�A�@��@��
@���@�C�@��@���@��+@�V@�-@�$�@��@��-@�x�@�X@�&�@��`@���@�j@�b@�P@~��@~�@~E�@}�-@}/@}�@|�@|Z@{��@z�@z=q@z-@z�@y��@x��@xQ�@w��@w�P@v�y@vE�@up�@t�@t��@s�
@sdZ@s"�@r�@q��@qx�@p�`@o�w@o|�@n��@n�@n��@nV@m��@m?}@m/@l�/@l9X@k�@kS�@kC�@kC�@ko@j��@i�^@i%@h�@h �@g��@g�P@gK�@fv�@e@e?}@eV@d�D@c��@b��@b�\@bn�@bM�@b=q@b-@b�@bJ@a��@a�^@aX@`Q�@_\)@_�@^ȴ@^��@^�+@^ff@^{@]@]�@]/@\�@\�@Zn�@Z=q@Y��@Yhs@X�`@X��@XbN@W�;@W�@V�+@VV@U�T@U@U�-@U`B@UV@T��@T�j@Tz�@T(�@S��@Sƨ@S��@R�H@R^5@Q�#@Qhs@Q&�@P��@P �@O��@Ol�@Nȴ@Nv�@Nff@N$�@N{@M�@MO�@L�/@L�D@L�@K��@K��@K�@Kt�@J�H@J~�@J-@JJ@JJ@Ix�@H�@H �@G�@G�@G\)@Fȴ@FV@F@E�@E�@D�@Dz�@DZ@D(�@C�
@C�@CS�@Co@B��@BM�@A�#@A7L@@��@@r�@@bN@@bN@@Q�@@ �@?�P@?;d@>��@>�+@>5?@>@=��@=�-@=?}@<�/@<�j@<�j@<Z@;��@;t�@;"�@:��@:�\@:�@9�@9��@9��@9x�@9G�@97L@9%@8�`@8A�@8b@7�;@7\)@7
=@6��@6E�@5�T@5�-@5�@5`B@5/@4�@4j@4(�@3ƨ@3C�@3o@2�@2�!@2��@2-@1��@1G�@0Ĝ@0A�@/�;@/��@/\)@/
=@.�+@.5?@.{@-p�@-O�@-/@,�/@,�@,z�@,Z@,I�@,9X@+ƨ@+t�@+dZ@+C�@+"�@+o@*��@*�\@*n�@*=q@*�@)��@)7L@(��@(�`@(�@(bN@(A�@'�@'��@'�@'��@'|�@&�R@&{@%��@%��@%p�@%/@$��@$�j@$�@$�D@$z�@$(�@#��@#�m@#��@#t�@#dZ@#C�@"��@"�!@"��@"�\@"~�@!�@!x�@!&�@!�@!�@ ��@ �@ bN@ Q�@  �@�@��@�w@�w@�w@��@|�@l�@+@��@��G�O�A˓uA˕�A˓uAˏ\A˟�A˟�A˕�A˝�A˴9AˬAˬAˮA˩�A˓uAˡ�A˧�AˑhA˕�A˕�AˑhAˏ\AˑhAˉ7AˋDA�t�A�|�A�x�A�v�A�r�A�x�A�t�A�v�A�z�A�v�A�v�A�z�A�v�A�n�A�p�A�n�A�hsA�`BA�\)A�`BA�`BA�\)A�XA�VA�ZA�VA�Q�A�O�A�O�A�K�A�M�A�O�A�O�A�Q�A�S�A�VA�O�A�O�A�S�A�A�A�E�A�E�A�A�A�A�A�C�A�?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	�B	��B	��B	��B	�B	�OB	�}B	�B	�B	��B	��B	�UB	��B	��B	�'B	�'B	��B	��B	�[B	��B	�aB	��B	��B	�UB	��B	��B	�qB	�qB	��B	�jB	ΥB	͟B	��B	�BB	�0B	̘B	ȴB	��B	ŢB	��B	�;B
	lB
IB
9$B
VB
[�B
J�B
IRB
V9B
ffB
^�B
�iB
�PB
��B
�[B
ҽB
�QB
��B
�lB/BB[BV�BlWB��B��B�XB��B�jB�+B��B��B��B�/B�B�B�yB��B�BB��BMBDB�B�B��B�B�;B��B�mB�NB��B��B��B�_BtBL�BC-B;�B8RB-B"B
�B
�,B
ÖB
�zB
�B
Q�B
49B
%FB
�B
B	��B	��B	�<B	�0B	�7B	�=B	wfB	k�B	IB	>BB	5�B	"�B	CB	+B	
=B�B�sB��B	B	B�B	XEB	��B	��B	�B	�FB	��B	�=B	�IB	��B	�7B	�zB	��B	خB	�)B	��B	�]B
B
4�B
JXB
MjB
P�B
T,B
W�B
[�B
^5B
_B
aHB
^5B
YB
YB
V9B
T�B
R�B
Q�B
P�B
N�B
K�B
IRB
E�B
B'B
B�B
C-B
E9B
B�B
DgB
D�B
EmB
I�B
M�B
MB
LdB
K�B
L�B
LdB
K�B
I�B
I�B
D�B
E�B
=qB
<�B
<B
;0B
9�B
5B
49B
5?B
3�B
4nB
4nB
4B
0�B
.�B
5�B
.�B
.}B
4�B
9�B
;�B
;dB
8B
8RB
<�B
?B
>wB
?HB
?HB
A�B
G�B
L�B
JXB
H�B
C-B
@B
A�B
@B
?}B
A B
@�B
@�B
=�B
<6B
<�B
>wB
C�B
A�B
=B
8�B
6�B
6B
9$B
@B
=�B
:�B
7�B
:^B
>�B
<B
:*B
6zB
3�B
33B
9$B
:�B
:^B
:^B
:*B
9�B
8RB
7�B
7LB
6�B
5�B
6B
49B
2�B
1�B
0�B
0�B
0UB
0UB
/�B
.B
/B
-�B
-CB
-CB
,B
,B
+�B
+kB
*�B
*�B
,�B
)�B
)*B
)*B
(�B
(XB
'�B
'�B
'RB
'B
%�B
%FB
$tB
"�B
!�B
 �B
!�B
�B
!B
�B
B
~B
IB
�B
CB
�B
�B
�B
B
B
xB
�B
	B
�B
�B
�B
1B
_B
1B
+B
+B
�B
SB
�B
�B
:B
:B
�B
\B
�B
JB
�B

rB

rB
	�B

�B
xB
~B
�B

	B

�B

	B
fB
1B
	7B
	B
fB
1B
�B
1B
1B
�B
	B
1B
�B
�B
+B
�B
+B
�B
fB
�B
�B
YB
�B
YB
+B
�B
SB
�B
uB
B
�B
uB
uB
�B
�B
B
�B
�B
�B
B
B
GB
{B
GB
GB
{B
B
B
�B
�B
�B
MB
MB
MB
�B
B
B
�B
B
SB
B
�B
�B
�B
�B
SB
�B
B
B
SB
�B
�B
�B
YB
%B
�B
�B
�B
�B
�B
	lB

=B

	B

	B
B
�B
xB
JB
JB
DB
DB
DB
xB
xB
~B
�B
�B
�B
�B
�B
�B
�B
.B
.B
.B
�B
bB
�B
.B
�B
 B
 B
�B
�B
�B
oB
:B
@B
uB
�B
B
{B
�B
FB
{B
�B
FB
�B
{B
�B
�B
MB
�B
�B
�B
SB
�B
�B
$B
YB
SB
�B
�B
�B
�B
�B
�B
_B
�B
�B
�B
7B
	B
1B
eB
�B
+B
�B
_B
$B
�B
�B
�B
+B
_B
�B
_B
�B
�B
�B
�B
B
�B
�B
xB
CB
�B
~B
�B
!B
!bB
!�B
!�B
"�B
"hB
"�B
!�B
"�B
#B
"hB
!�B
"�B
#B
"�B
"hB
"4B
"4B
!�B
"hB
"�B
"�B
#�B
#�B
#�B
#�B
#B
#�B
#nB
#nB
#�B
#B
$�B
$�B
$�B
%FB
%B
%B
%zB
%�B
&�B
$�B
&�B
'�B
'RB
(XB
'�B
'RB
(�B
(�B
)_B
($B
'B
%�B
&LB
&B
'B
'B
'B
(XB
)_B
)_B
+6B
*�B
)�B
*�B
,qB
,B
+kB
,�B
+�B
,B
+�B
+�B
+�B
,�B
-wB
.}B
.B
.}B
.�B
/B
/OB
/�B
0!B
0�B
0UB
0UB
0�B
0�B
/�B
/�B
0!B
0UB
1�B
1'B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
2aB
2�B
2�B
33B
2�B
49B
4B
3�B
4nB
49B
4B
3�B
3�B
4�B
5tB
6B
6�B
6zB
6�B
7LB
7LB
7�B
7�B
7�B
8�B
8�B
:*B
:^B
:^B
;�B
;0B
<B
<�B
<�B
=qB
>B
=B
=qB
=<B
=qB
=�B
>B
>�B
?}B
?HB
?}B
@B
@�B
@�B
@�B
@OB
@B
@�B
A�B
A�B
A�B
B�B
B�B
B[B
B'B
B�B
CaB
C�B
C�B
C�B
E9B
E�B
E�B
E�B
E�B
FB
F?B
FtB
FB
F?B
F?B
FB
GzB
HB
HB
H�B
H�B
H�B
H�B
I�B
I�B
JXB
JXB
I�B
J�B
J�B
K)B
K�B
K�B
K�B
K�B
K�B
LdB
MB
MB
MB
M6B
MB
M6B
M�B
M�B
M�B
M�B
NB
M�B
NB
M�B
NB
OB
O�B
O�B
P�B
P}B
QNB
Q�B
RTB
R�B
R�B
R�B
R�B
R�B
RTB
RTB
S&B
S[B
S�B
T�B
T�B
T�B
TaB
T,B
T�B
T�B
U2B
T�B
UgB
V9B
V9B
U�B
U�B
VB
V9B
V�B
V�B
V�B
W?B
W?B
WsB
W�B
W�B
XB
XEB
X�B
YB
YB
YB
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
ZQB
[�B
[WB
[�B
\)B
\)B
\)B
\]B
\�B
]dB
]dB
]dB
]/B
]�B
^jB
^5B
^�B
^�B
^�B
_;B
_;B
_;B
_�B
_�B
_pB
_�B
_�B
_�B
`�B
`�B
`vB
aB
aHB
a�B
bB
bB
bB
bNB
bNB
bNB
b�B
b�B
c B
c�B
dZB
d�B
d�B
d�B
d�B
e,B
e`B
e,B
f2B
f�B
f�B
gB
g8B
g8B
g�B
g�B
g8B
h>B
h
B
hsB
h�B
iB
i�B
i�B
i�B
iyB
jB
j�B
j�B
j�B
j�B
jB
kB
kQB
k�B
k�B
k�B
k�B
l�B
l�B
lWB
l�B
m)B
m]B
m�B
m�B
m�B
m]B
m)B
ncB
n�B
n�B
n�B
o5B
o�B
p;B
p;B
p;B
p;B
p;B
p�B
p�B
p�B
qB
qB
qB
qB
q�B
q�B
q�B
q�B
qvB
r|B
r�B
r�B
r|B
r|B
r|B
sB
sB
sB
s�B
s�B
s�B
s�B
s�B
tB
tTB
tB
s�B
t�B
t�B
t�B
u%B	�B	�B	�IB	��B	��B	�B	��B	��B	��B	�UB	�qB	��B	�B	��B	��B	�6B	�B	�wB	�CB	�IB	��B	��B	��B	��B	��B	�wB	��B	�UB	��B	�wB	�IB	��B	�CB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�OB	�[B	��B	��B	�'B	�!B	�UB	�'B	��B	��B	��B	��B	��B	�!B	�[B	�UB	��B	�aB	��B	��B	�aB	�[B	�!B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   B	�lB	��B	�aB	� B	�6B	��B	�uB	��B	�FB	��B	�B	�B	��B	��B	�0B	�FB	�YB	��B	��B	�kB	��B	��B	�~B	��B	��B	�]B	��B	��B	��B	ʭB	�bB	��B	ͩB	�5B	ϰB	�bB	�qB	��B	ìB	͝B	�%B	��B
	sB
�B
;�B
X�B
^@B
L
B
I�B
VB
hEB
`B
�}B
��B
��B
��B
�B
�5B
�#B�B;�BL�BarBy�B��B�B�BB�QB�UB�NB_B B��B�;B��B��B�nB��B�}B��B	*B�B�BB �B�B�_B�?B�SBהB�-B��B��B�,B}dBP�BF�B?>BFB<B�B
��B
�B
ʢB
��B
��B
[�B
;�B
+<B
�B

B	�TB	��B	��B	�;B	�B	��B	�B	x�B	L�B	C�B	>�B	%pB	!zB	&�B	�B�;B� B�FB	~B	FvB	Y�B	�~B	�B	��B	��B	�NB	�&B	��B	��B	��B	��B	��B	�B	�8B	��B	�uB
�B
3�B
J7B
M�B
Q�B
U�B
Y5B
]B
`�B
c�B
h<B
a�B
\B
[�B
X(B
V0B
SrB
R`B
R�B
P�B
N�B
N�B
GVB
D�B
C�B
F�B
GcB
EB
E�B
E�B
F&B
J;B
NCB
M�B
M�B
M�B
N'B
M�B
M�B
LcB
MlB
J�B
HeB
>TB
=�B
>B
?1B
>PB
6�B
6�B
7�B
4B
4�B
5�B
5�B
2pB
2�B
:cB
/vB
.	B
4bB
:VB
<�B
=�B
8mB
8B
=5B
?�B
@B
@mB
?'B
AB
G�B
OB
LzB
LzB
D�B
@�B
B�B
AB
@�B
BB
A�B
BmB
?<B
=mB
<�B
?B
F�B
E5B
?�B
:�B
7zB
5�B
9�B
B�B
@pB
<uB
7�B
;�B
A�B
?nB
=�B
9�B
4�B
3vB
:B
;�B
;HB
<B
<B
;DB
9�B
8�B
84B
8`B
8�B
8�B
5dB
4B
2�B
1
B
1�B
1NB
1�B
0�B
0
B
0nB
/ B
.�B
.�B
,�B
,XB
+�B
+B
+:B
,�B
/�B
*3B
*6B
*FB
),B
(�B
(�B
)�B
)B
(�B
'�B
(B
&TB
#^B
"\B
"�B
#�B
 �B
 6B
�B
�B
B
�B
�B
(B
�B
�B
�B
�B
�B
�B
�B
�B
0B
B
!B
B
�B
�B
�B
�B
B
�B
oB
�B
[B
B
iB
gB
`B
-B
�B
}B
�B
B
�B
#B
�B
LB

�B
fB

�B
�B
	)B

5B

(B
	�B
	�B

B
	�B
	,B
	kB

�B
�B
�B
$B
�B
�B
	,B

�B
	�B
	 B
	B
�B
�B
;B
	LB
%B
�B
�B
�B
�B
\B
nB
�B
_B
B
B
�B
AB
B
�B
�B
�B
B
B

B
�B
�B
_B
B
�B
�B
�B
�B
B
iB
	B
bB
�B
^B
YB
�B
�B
B
4B
B
�B
�B
TB
�B
;B
�B
�B
�B
�B
�B
�B
'B
�B
BB
	�B

`B

�B

zB

�B
B
pB
2B
�B
B
lB
�B
�B
B
yB
yB
�B
4B
�B
UB
�B
�B
HB
�B
�B
�B
zB
�B
�B
B
xB
_B
�B
B
yB
�B
�B
9B
B
�B
4B
�B
	B
BB
�B
	B
B
�B
�B
 B
�B
KB
0B
�B
tB
�B
-B
�B
�B
�B
�B
�B
MB
WB
GB
�B
WB
�B
!B
�B
EB
'B
B
�B
�B
6B
�B
�B
�B
�B
�B
tB
7B
�B
.B
3B
dB
�B
B
1B
cB
%B
�B
�B
]B
�B
�B
*B
�B
!B
4B
"'B
"�B
#7B
#qB
#�B
#�B
"�B
#jB
#qB
"B
"`B
#�B
$pB
$'B
#�B
#JB
"�B
!�B
"�B
"�B
#�B
%B
$�B
$�B
$qB
#WB
#�B
#vB
#�B
#�B
#�B
%GB
$�B
$�B
%B
%�B
%�B
&�B
'�B
'�B
$�B
')B
(�B
(B
)�B
'�B
'�B
)zB
)�B
+B
)�B
'�B
&�B
'TB
'LB
'�B
'�B
(}B
(�B
)oB
)�B
,B
*�B
*uB
+!B
,�B
,�B
,SB
-+B
,;B
,ZB
,6B
+�B
,'B
,�B
.:B
.�B
.4B
.�B
.�B
/�B
0#B
/�B
1IB
1�B
0�B
1B
1=B
0�B
0�B
0�B
0�B
1"B
2B
1�B
1RB
0�B
1B
1YB
2B
1�B
2GB
2�B
34B
3�B
4B
3�B
4�B
4JB
4�B
5B
4�B
4*B
4 B
4�B
5|B
6LB
6�B
6�B
6�B
7FB
8?B
7�B
8�B
8LB
8{B
9�B
9�B
:�B
:�B
;qB
<RB
;�B
=B
<�B
=wB
>>B
?:B
=iB
=�B
=dB
=�B
>B
>�B
?B
?�B
?�B
@B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
BYB
BfB
BNB
B�B
B�B
B�B
B�B
C�B
C�B
D2B
DB
D�B
E�B
E�B
E�B
E�B
E�B
FB
FRB
F�B
F#B
FB
F�B
F�B
HJB
HWB
HcB
H�B
H�B
H�B
I6B
JB
I�B
J�B
J�B
J�B
LWB
K2B
KlB
L'B
L�B
LDB
LB
L[B
M/B
M�B
MBB
MsB
M]B
M B
M�B
M�B
NB
M�B
NB
N]B
NB
N?B
NB
N�B
O�B
P5B
P[B
Q2B
QB
Q�B
RKB
R�B
S+B
SGB
SB
S4B
R�B
R�B
SB
S�B
S�B
TrB
UB
T�B
T�B
T�B
T�B
U3B
USB
UVB
T�B
VB
W'B
V�B
VB
VB
VfB
V�B
WOB
W5B
W*B
W�B
WB
W�B
XB
XB
XhB
X�B
X�B
YhB
YpB
Y�B
Z;B
ZrB
Z�B
Z�B
[B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
\B
\�B
\gB
\eB
\�B
]B
]�B
]�B
]xB
]�B
^�B
^�B
^�B
_3B
_'B
_B
_tB
_gB
_vB
_�B
_�B
_�B
`B
_�B
`�B
aB
`�B
a	B
ayB
a�B
bB
b�B
bSB
bTB
b{B
b�B
b�B
c9B
c>B
c�B
dJB
d�B
d�B
d�B
eB
eMB
e�B
e�B
e�B
f�B
g=B
gB
gVB
g�B
g�B
h0B
g�B
g�B
hkB
h>B
h�B
iB
iMB
i�B
i�B
j	B
i�B
j�B
j�B
j�B
kB
j�B
j�B
kiB
k�B
k�B
k�B
l&B
l�B
l�B
l�B
l�B
m!B
m\B
m�B
m�B
m�B
m�B
m�B
nB
oB
n�B
oB
oB
o�B
p?B
pSB
pUB
pdB
p\B
p�B
qB
p�B
p�B
qFB
q*B
qBB
q�B
rB
q�B
q�B
q�B
r!B
s B
sB
r�B
r�B
r�B
r�B
sBB
s6B
sWB
s�B
s�B
s�B
s�B
s�B
tIB
t}B
t>B
t=B
u#B
t�B
t�G�O�B	�B	�B	�IB	��B	��B	�B	��B	��B	��B	�UB	�qB	��B	�B	��B	��B	�6B	�B	�wB	�CB	�IB	��B	��B	��B	��B	��B	�wB	��B	�UB	��B	�wB	�IB	��B	�CB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�OB	�[B	��B	��B	�'B	�!B	�UB	�'B	��B	��B	��B	��B	��B	�!B	�[B	�UB	��B	�aB	��B	��B	�aB	�[B	�!B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�#<V��<4<<]�<b(<#�
<#�
<#�
<#�
<=ߜ<#�
<#�
<#�
<#�
<#�
<#�
<#�
</�?<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Z�<%�v<#�
<#�
<#�
<h?h<{�m<bĭ<#�
<(k�<#�
<#�
<�_<. �<#�
<#�
<#�
<#�
<#�
<#�
<#�
<0��<#�
<#�
<#�
<ct�<#�
<#�
<#�
<#�
<#�
<v�<fg�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021050101174920210501011749IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021051102004920210511020049QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021051102004920210511020049QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364720220422133647IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064220220426160642IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064220220426160642IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064220220426160642IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                
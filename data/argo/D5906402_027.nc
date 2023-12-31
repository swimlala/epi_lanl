CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-08-09T05:33:05Z creation; 2022-04-26T16:07:00Z DMQC;      
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
_FillValue        G�O�     P  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  ZX   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     P  a�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  ~�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     P  �P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     P  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     P  �D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     P  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     P 
8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T '�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     P .�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T L,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     P S�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` p�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   q0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   w0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   }0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210809053305  20220426232408  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_027                 8138_008904_027                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @يJ��-�@يJ��-�11  @يJ͞��@يJ͞��@->x?��@->x?���d�=Ć�.�d�=Ć�.11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@@  @�  @�  @��R@޸R@��RA  A ��A,��A@��A_\)A�  A��A�  A��A��A�Q�A�  A�Q�B (�B  B  BQ�B   B'�
B/�
B8  B?�
BG�BO�
BX  B`  Bh  Bp  Bx  B�  B��B��B�  B�(�B�{B��B�  B�(�B�ffB�  B�  B�  B�  B�  B��B�{B�{B�  B��B��B��
B��B�  B�  B�  B�{B�{B�  B�  B�{B��C   C
=C
=C  C  C

=C  C
=C  C  C
=C
=C
=C  C  C��C�C!�C#��C%��C(  C*
=C+��C-��C/��C2  C3��C5��C8
=C:
=C<
=C>
=C@{CB�CD
=CE��CH  CI��CK��CN  CP  CR
=CT  CU��CX  CZ  C\  C^  C`
=Cb
=Cd  Cf  Ch  Cj  Ck��Cn  Cp  Cr  Ct  Cv  Cx
=Cz  C{��C}��C��C���C�  C�  C�  C�C�  C���C���C�  C�  C�C�  C���C�  C�  C�  C�  C�  C�C�C�  C�  C�C�C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�C�C�C�  C�  C�C�  C���C���C���C���C���C�  C�  C���C���C���C���C�  C�  C�  C�C�  C���C�  C�  C�  C�  C�  C�  C���C���C�  C�  C���C�  C�C�  C���C���C���C�C�C�C�C�C�C�C�C�
=C�  C���C���C���C�  C�  C�C�  C���C�  C�  C�  C���C�C�  C�  C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�  C�  C���C�C�C�  C�  C�  C���C�  C�
=C�C�  C���D   D ��D �qD}qD�qD� D�qD� D�D�D�D� D  D� D�qD}qD  D��D	�D	� D
  D
}qD  D� D�qD� D  D� D�qD� D�D��D�qD}qD  Dz�D�qD��D  D}qD�D� D��D� D�D��D  D��D  D� D  D� DD� D�D}qD  D�D�D� D�qD}qD  D}qD��D }qD!�D!� D!��D"� D#�D#� D#�qD$z�D%  D%�D&�D&��D'  D'� D(  D(��D)�D)��D*  D*}qD*�qD+}qD+�qD,}qD,�qD-� D.  D.z�D.��D/� D/�qD0z�D0�qD1� D2  D2� D3  D3}qD3�qD4� D5�D5� D5�qD6� D7�D7��D8�D8}qD9  D9�D:  D:� D:�qD;� D<  D<� D=  D=� D>  D>� D?  D?� D?�qD@� DA�DA��DB�DB��DC  DC}qDD  DD��DE�DE}qDF  DF��DG  DG��DH  DH� DI  DI}qDI�qDJ� DK  DK}qDK�qDL� DM�DM��DN�DN� DN�qDO��DP�DP� DP�qDQ}qDR  DR��DS�DS� DT  DT� DU  DU� DV�DV� DW  DW� DX  DX��DYDY��DZ  DZ� D[  D[��D\�D\��D]  D]� D^  D^��D_�D_��D`  D`� Da  Da}qDb  Db��Dc�Dc� Dd  Dd� Dd��Dez�De�qDf� Dg  Dg� Dh�Dh� Di�Di��Dj  Dj� Dk  Dk� Dl�Dl��Dm  Dm� Dn�Dn}qDn�qDo}qDp  Dp��Dq�Dq��Dr  Dr� Ds  Ds}qDt  Dt� Du�Du� Du��Dv}qDw  Dw� Dx�Dx� Dx�qDy}qDz  Dz}qD{  D{}qD{��D|� D|�qD}}qD~  D~� D~�qD��D�  D�>�D�� D���D��qD�@ D�~�D���D�  D�AHD��HD��HD���D�>�D�}qD���D�  D�@ D��HD���D�  D�AHD�� D��HD�HD�AHD��HD��HD�HD�>�D�� D�� D���D�>�D�� D�� D�HD�@ D��HD�� D���D�@ D��HD��HD�HD�@ D�� D�� D���D�@ D��HD�� D���D�@ D��HD�� D�  D�>�D��HD�� D�  D�@ D�� D�� D���D�@ D�~�D�� D�  D�@ D�~�D���D���D�AHD�� D�� D�  D�@ D��HD�� D�HD�AHD�� D��HD��D�@ D�� D�� D���D�@ D��HD��HD�HD�AHD��HD��HD�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�}qD���D���D�>�D�~�D��HD��D�AHD�� D���D�  D�B�D��HD�� D�  D�@ D��HD���D��)D�=qD��HD��HD���D�@ D�� D�� D�HD�@ D�~�D�� D�  D�>�D�~�D���D�  D�AHD�� D���D�  D�AHD�� D�� D�  D�@ D�� D���D���D�@ D��HD�� D���D�>�D�� D��HD�  D�@ D�� D��HD�HD�@ D�� D��HD�HD�@ D�~�D�� D�  D�@ D�~�D���D���D�>�D�~�D�� D�  D�>�D�� D��HD�HD�AHD��HD��HD�  D�AHD��HD�� D���D�@ D�� D���D���D�AHD��HD�� D���D�>�D�� D�D�HD�@ D�� D�� D���D�@ D��HD��HD�  D�@ D�� D���D�  D�AHD�� D���D�HD�@ D�~�D�� D���D�>�D�� D�� D���D�@ D��HD��HD�HD�AHD��HD�� D�  D�>�D�~�D���D���D�>�D�~�D�� D�HD�@ D�� D���D��qD�@ D��HD��HD�HD�>�D�� D�� D�  D�AHD�~�D��qD���D�>�D�� D��HD�HD�@ DHD��HD�  D�>�DÀ D�� D�  D�>�D�~�Dľ�D�HD�@ Dŀ D�� D�  D�AHDƀ D�� D�HD�AHDǂ�D�� D���D�>�DȀ D��HD�HD�@ Dɀ D��HD�HD�AHD�~�D��HD���D�>�DˁHD�� D���D�AHD̀ D�� D�  D�AHD̀ D�� D��D�AHD΀ D�� D�  D�AHDρHD�� D���D�>�DЀ D�� D�  D�@ D�~�D�� D�  D�@ DҀ D�� D�  D�@ D�~�DӾ�D�HD�AHDԀ D�� D���D�B�DՁHDվ�D�  D�@ DցHD�� D���D�AHDׂ�D��HD�HD�@ D؁HD��HD�  D�B�Dق�D�� D�  D�@ Dڀ D�� D�  D�>�D�~�D�� D���D�=qD܀ D�� D���D�@ D݁HD�� D�  D�AHDށHD�� D�  D�@ D�~�D߾�D�  D�@ D��HDྸD�  D�@ D�HD�D�  D�@ D�HD��HD��D�>�D� D�D��D�@ D�~�D�� D�HD�AHD� D�qD���D�AHD悏D澸D���D�@ D� D��HD�  D�@ D� D�D��?�?#�
?aG�?�\)?��
?�p�?��?�@�@\)@(�@(��@8Q�@@  @L��@W
=@fff@u@�  @��@�{@�
=@�  @��@���@�Q�@�G�@�=q@�33@��H@�\@���@�z�@��RA33AQ�A��A��AA=qA�RA#33A'
=A,(�A/\)A4z�A8Q�A<(�A@��ADz�AH��AL(�AP  ATz�AW�A\(�A_\)Ac33Ag
=Aj=qAn�RAq�AuAz=qA}p�A���A��HA�z�A��RA���A�=qA���A�
=A���A��HA��A�
=A���A��
A�A�Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ?��@�\@@  @�  @�  @��R@޸R@��RA  A ��A,��A@��A_\)A�  A��A�  A��A��A�Q�A�  A�Q�B (�B  B  BQ�B   B'�
B/�
B8  B?�
BG�BO�
BX  B`  Bh  Bp  Bx  B�  B��B��B�  B�(�B�{B��B�  B�(�B�ffB�  B�  B�  B�  B�  B��B�{B�{B�  B��B��B��
B��B�  B�  B�  B�{B�{B�  B�  B�{B��C   C
=C
=C  C  C

=C  C
=C  C  C
=C
=C
=C  C  C��C�C!�C#��C%��C(  C*
=C+��C-��C/��C2  C3��C5��C8
=C:
=C<
=C>
=C@{CB�CD
=CE��CH  CI��CK��CN  CP  CR
=CT  CU��CX  CZ  C\  C^  C`
=Cb
=Cd  Cf  Ch  Cj  Ck��Cn  Cp  Cr  Ct  Cv  Cx
=Cz  C{��C}��C��C���C�  C�  C�  C�C�  C���C���C�  C�  C�C�  C���C�  C�  C�  C�  C�  C�C�C�  C�  C�C�C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�C�C�C�  C�  C�C�  C���C���C���C���C���C�  C�  C���C���C���C���C�  C�  C�  C�C�  C���C�  C�  C�  C�  C�  C�  C���C���C�  C�  C���C�  C�C�  C���C���C���C�C�C�C�C�C�C�C�C�
=C�  C���C���C���C�  C�  C�C�  C���C�  C�  C�  C���C�C�  C�  C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�  C�  C���C�C�C�  C�  C�  C���C�  C�
=C�C�  C���D   D ��D �qD}qD�qD� D�qD� D�D�D�D� D  D� D�qD}qD  D��D	�D	� D
  D
}qD  D� D�qD� D  D� D�qD� D�D��D�qD}qD  Dz�D�qD��D  D}qD�D� D��D� D�D��D  D��D  D� D  D� DD� D�D}qD  D�D�D� D�qD}qD  D}qD��D }qD!�D!� D!��D"� D#�D#� D#�qD$z�D%  D%�D&�D&��D'  D'� D(  D(��D)�D)��D*  D*}qD*�qD+}qD+�qD,}qD,�qD-� D.  D.z�D.��D/� D/�qD0z�D0�qD1� D2  D2� D3  D3}qD3�qD4� D5�D5� D5�qD6� D7�D7��D8�D8}qD9  D9�D:  D:� D:�qD;� D<  D<� D=  D=� D>  D>� D?  D?� D?�qD@� DA�DA��DB�DB��DC  DC}qDD  DD��DE�DE}qDF  DF��DG  DG��DH  DH� DI  DI}qDI�qDJ� DK  DK}qDK�qDL� DM�DM��DN�DN� DN�qDO��DP�DP� DP�qDQ}qDR  DR��DS�DS� DT  DT� DU  DU� DV�DV� DW  DW� DX  DX��DYDY��DZ  DZ� D[  D[��D\�D\��D]  D]� D^  D^��D_�D_��D`  D`� Da  Da}qDb  Db��Dc�Dc� Dd  Dd� Dd��Dez�De�qDf� Dg  Dg� Dh�Dh� Di�Di��Dj  Dj� Dk  Dk� Dl�Dl��Dm  Dm� Dn�Dn}qDn�qDo}qDp  Dp��Dq�Dq��Dr  Dr� Ds  Ds}qDt  Dt� Du�Du� Du��Dv}qDw  Dw� Dx�Dx� Dx�qDy}qDz  Dz}qD{  D{}qD{��D|� D|�qD}}qD~  D~� D~�qD��D�  D�>�D�� D���D��qD�@ D�~�D���D�  D�AHD��HD��HD���D�>�D�}qD���D�  D�@ D��HD���D�  D�AHD�� D��HD�HD�AHD��HD��HD�HD�>�D�� D�� D���D�>�D�� D�� D�HD�@ D��HD�� D���D�@ D��HD��HD�HD�@ D�� D�� D���D�@ D��HD�� D���D�@ D��HD�� D�  D�>�D��HD�� D�  D�@ D�� D�� D���D�@ D�~�D�� D�  D�@ D�~�D���D���D�AHD�� D�� D�  D�@ D��HD�� D�HD�AHD�� D��HD��D�@ D�� D�� D���D�@ D��HD��HD�HD�AHD��HD��HD�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�}qD���D���D�>�D�~�D��HD��D�AHD�� D���D�  D�B�D��HD�� D�  D�@ D��HD���D��)D�=qD��HD��HD���D�@ D�� D�� D�HD�@ D�~�D�� D�  D�>�D�~�D���D�  D�AHD�� D���D�  D�AHD�� D�� D�  D�@ D�� D���D���D�@ D��HD�� D���D�>�D�� D��HD�  D�@ D�� D��HD�HD�@ D�� D��HD�HD�@ D�~�D�� D�  D�@ D�~�D���D���D�>�D�~�D�� D�  D�>�D�� D��HD�HD�AHD��HD��HD�  D�AHD��HD�� D���D�@ D�� D���D���D�AHD��HD�� D���D�>�D�� D�D�HD�@ D�� D�� D���D�@ D��HD��HD�  D�@ D�� D���D�  D�AHD�� D���D�HD�@ D�~�D�� D���D�>�D�� D�� D���D�@ D��HD��HD�HD�AHD��HD�� D�  D�>�D�~�D���D���D�>�D�~�D�� D�HD�@ D�� D���D��qD�@ D��HD��HD�HD�>�D�� D�� D�  D�AHD�~�D��qD���D�>�D�� D��HD�HD�@ DHD��HD�  D�>�DÀ D�� D�  D�>�D�~�Dľ�D�HD�@ Dŀ D�� D�  D�AHDƀ D�� D�HD�AHDǂ�D�� D���D�>�DȀ D��HD�HD�@ Dɀ D��HD�HD�AHD�~�D��HD���D�>�DˁHD�� D���D�AHD̀ D�� D�  D�AHD̀ D�� D��D�AHD΀ D�� D�  D�AHDρHD�� D���D�>�DЀ D�� D�  D�@ D�~�D�� D�  D�@ DҀ D�� D�  D�@ D�~�DӾ�D�HD�AHDԀ D�� D���D�B�DՁHDվ�D�  D�@ DցHD�� D���D�AHDׂ�D��HD�HD�@ D؁HD��HD�  D�B�Dق�D�� D�  D�@ Dڀ D�� D�  D�>�D�~�D�� D���D�=qD܀ D�� D���D�@ D݁HD�� D�  D�AHDށHD�� D�  D�@ D�~�D߾�D�  D�@ D��HDྸD�  D�@ D�HD�D�  D�@ D�HD��HD��D�>�D� D�D��D�@ D�~�D�� D�HD�AHD� D�qD���D�AHD悏D澸D���D�@ D� D��HD�  D�@ D� D�G�O�?�?#�
?aG�?�\)?��
?�p�?��?�@�@\)@(�@(��@8Q�@@  @L��@W
=@fff@u@�  @��@�{@�
=@�  @��@���@�Q�@�G�@�=q@�33@��H@�\@���@�z�@��RA33AQ�A��A��AA=qA�RA#33A'
=A,(�A/\)A4z�A8Q�A<(�A@��ADz�AH��AL(�AP  ATz�AW�A\(�A_\)Ac33Ag
=Aj=qAn�RAq�AuAz=qA}p�A���A��HA�z�A��RA���A�=qA���A�
=A���A��HA��A�
=A���A��
A�A�Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AڬAڰ!AڮAڬAڮAڲ-Aڲ-Aڰ!AڮAڰ!AڮAڮAک�AڶFAڲ-Aڧ�AڮAڝ�Aڛ�Aڕ�Aډ7A�x�A�hsA�\)A�^5A�ZA�^5A�dZA�^5A�bNA�dZA�bNA�^5A�VA�I�A�K�A�O�A�O�A�I�A�E�A�?}A�/A�bAټjA�%A���A�$�A�XA�AУ�A��#A�
=A��TAɰ!Aȇ+A�A���Aĥ�A��A��+A�A�A�ffA���A��yA��A�dZA��A��A�t�A���A�E�A�-A�G�A��mA�O�A�ffA�v�A��9A�%A�VA�&�A�VA��A�VA��A�~�A�n�A��-A��A�ĜA��A���A�I�A���A�|�A�;dA���A��A�`BA��RA}��Az��Aw�Ar�+Ao�TAm�mAj��Ah~�Ag�AfM�AeXAd�\A`�A[�wAZ5?AX��AUK�AShsAR(�AM��AIAH{AFZABVA<$�A9��A9�A8{A7XA77LA6��A6�jA5��A41A2��A1�wA0~�A//A.^5A.�A.A�A,�A,$�A+��A+/A*�HA+?}A+C�A*��A)x�A((�A&��A&�A$�A$��A$v�A$JA#�hA#VA"�`A"��A"��A"��A"ffA!�TA!dZA Ap�A�A�yAĜAQ�A33AM�A�mAt�A�HA~�A-A��AVA�9AM�AJA��A��AK�A��A�
AS�A�AA��A�/A��A��Az�A1'A��At�A�uA��AhsAĜA�AG�A��AQ�A^5AQ�A(�A��A�-AC�A�AĜAn�A5?A��A��AoA
��A
�+A
�DA
�DA
5?A	�-A	|�A	�A	XA	O�A	?}A	oA�jAAXA7LA"�A��A�`A��A�A�AXA�A��A��A��An�AjAbNA�A ��A @��m@���@���@���@�ƨ@��y@�%@�
=@�\)@��@�@��@�/@�I�@�b@�F@���@��T@�%@�A�@�ƨ@�C�@��@�+@땁@�
=@��@��@陚@�7L@��`@�@�A�@��;@睲@��@�ff@��@��m@�|�@��@��H@���@�\@�^5@��@�G�@�j@��u@�1@�+@ް!@ާ�@�V@�{@���@�@ݺ^@ݡ�@ݑh@ܼj@�(�@۶F@�l�@�\)@�;d@�
=@�n�@١�@�%@�r�@��
@�+@�M�@�X@���@�(�@�C�@�E�@��T@Ѳ-@љ�@�`B@���@�r�@� �@�1@�  @��;@ϝ�@�C�@��@�
=@��@θR@�~�@́@�%@��/@�z�@�(�@˕�@�+@�o@���@�ȴ@�5?@�@ɉ7@�O�@���@�(�@ǝ�@�C�@�+@��y@�ȴ@Ɨ�@�ff@�$�@Ų-@���@�  @þw@Õ�@�S�@�@�ȴ@°!@�ff@�@���@�&�@�1'@�
=@�5?@�J@��@�@�p�@�&�@���@�Q�@��m@�t�@���@�5?@��T@��@��j@�r�@�1@��F@�S�@��!@�n�@��@��@��T@�@��@�I�@��m@���@�K�@��@��\@�E�@���@�p�@�X@�V@���@�33@�~�@��7@��D@�(�@�  @��@�S�@��@�M�@��@��h@�&�@���@�Ĝ@�r�@�I�@��@�t�@��\@�-@�@��7@�O�@�/@��/@�9X@���@��@�^5@�M�@�E�@�=q@�-@��@�@��T@���@��j@�I�@�1'@�(�@��@���@�l�@�C�@�
=@��!@�J@�X@��u@�Q�@�I�@�9X@� �@�b@��
@�33@��+@��#@��h@�x�@�`B@�X@�X@�X@�?}@���@��/@��j@��@�(�@�  @���@��w@��@�dZ@�@�ȴ@���@�n�@�=q@��@��T@���@��/@���@��D@�j@�A�@��@��w@��P@�K�@�+@�"�@��@��R@�n�@�J@�/@�z�@��;@��P@�
=@�~�@�=q@���@��-@�X@��@�r�@���@��m@��w@�t�@���@�v�@�E�@�J@��#@���@��7@�x�@�p�@�`B@�O�@�V@��`@��D@�9X@� �@���@��
@���@�K�@���@���@���@��+@�ff@�-@��@��-@�`B@�/@���@��/@���@��j@��D@�A�@�S�@�ȴ@���@�n�@�M�@�{@�J@��T@���@���@���@��h@��7@�`B@��@���@��j@��9@��@~v�@}�@|�/@{��@{�@{t�@{S�@zM�@yhs@x�`@xĜ@x��@x�@xbN@x  @v�R@u�@uO�@t�@s�@r�H@rn�@rM�@rM�@r=q@r=q@r�@r�@q��@p�@p �@p  @o��@o��@o�w@o;d@nv�@m@l��@l9X@k�F@kdZ@kdZ@kC�@ko@j=q@i�7@ix�@iG�@i&�@i&�@i&�@i%@h�`@h�9@hr�@hbN@hA�@g�w@fȴ@fV@fE�@fE�@fE�@fE�@f5?@e�T@e/@d�/@dZ@d9X@ct�@bn�@bJ@a��@`��@` �@_��@^�@]�-@\�/@\��@\��@\z�@\1@[��@[dZ@[dZ@[dZ@[dZ@[dZ@[dZ@[dZ@Z�H@Y��@Yx�@Y7L@Y%@XĜ@X��@XA�@X  @W��@W�w@W�@W�@W|�@WK�@V�@Vff@U�-@U?}@T��@T1@S"�@R�\@RM�@Q�@Q��@Q��@Q��@Q�7@Qhs@Pr�@O��@O��@O\)@N�R@Nv�@M�T@M`B@MV@L�j@K�F@J�!@J��@J�\@J~�@Jn�@JM�@I�@I��@I%@H�u@H  @F�@F5?@E�-@EO�@E�@D�/@D��@Dj@C�m@B�H@B~�@B^5@BJ@@��@@Q�@?��@?�P@>�y@>�+@>ff@>ff@>E�@>5?@>@=O�@<�j@<j@<9X@;�
@;��@;�@;t�@;33@:�H@:��@:��@:n�@:J@9��@9��@9x�@9&�@81'@7�w@7l�@7�@7�@6�y@6�R@65?@5@5�@5�@4�@4�/@4��@4�D@4(�@3��@3"�@3@2~�@2^5@2M�@2J@1��@1�@1�#@1��@1X@1G�@1&�@0��@0��@0�@0 �@/�@/��@/�w@/|�@/K�@/;d@/+@/+@.��@.��@.v�@.V@.5?@.5?@.5?@.{@-�-@-p�@-O�@-O�@-?}@-/@-�@,��@,�/@,��@,j@,Z@,�@+ƨ@+dZ@+S�@+33@+o@+@*�@*��@*J@*J@)�#@)�7@)X@)�@(��@(��@(bN@(A�@(b@(  @(  @'�@'�@'K�@'�@&��@%�T@%�h@%/@%V@$��@$�@$�@$�@$�@$�j@$9X@#�
@#ƨ@#�F@#�@#"�@#o@#@"�H@"�H@"��@"��@"�!@"�!@"��@"^5@"=q@"-@!��@!�@ r�@ 1'@ b@   @�@�@�P@�P@\)@��@ff@$�@��@p�@�/@�j@Z@��@33@�@��@��@�\@n�@n�@^5@M�@�#@�^@�7@hs@7L@�@�`Aڥ�Aڧ�Aڰ!Aک�Aڰ!Aڲ-Aڰ!Aڴ9AڮAڲ-Aڰ!AڮAڰ!Aک�AڮAڬAڮAک�AڬAڬAک�Aڰ!AڮAڰ!Aڰ!Aڲ-Aڴ9Aڰ!Aڴ9Aڰ!Aڲ-Aڰ!Aڰ!Aڲ-AڮAڰ!AڬAڮAڰ!AڬAڲ-AڬAڰ!AڮAڮAڲ-AڬAڰ!AڬAڬAڬAک�AڮAڥ�AڮAڬAڥ�Aڣ�Aڥ�Aڣ�Aڴ9Aڲ-AڶFAڴ9Aڴ9AڸRAڴ9Aڴ9AڸRAڴ9AڶFAڶFAڲ-AڮAڲ-AڮAڮAڲ-Aڛ�Aڥ�AڮG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         AڬAڰ!AڮAڬAڮAڲ-Aڲ-Aڰ!AڮAڰ!AڮAڮAک�AڶFAڲ-Aڧ�AڮAڝ�Aڛ�Aڕ�Aډ7A�x�A�hsA�\)A�^5A�ZA�^5A�dZA�^5A�bNA�dZA�bNA�^5A�VA�I�A�K�A�O�A�O�A�I�A�E�A�?}A�/A�bAټjA�%A���A�$�A�XA�AУ�A��#A�
=A��TAɰ!Aȇ+A�A���Aĥ�A��A��+A�A�A�ffA���A��yA��A�dZA��A��A�t�A���A�E�A�-A�G�A��mA�O�A�ffA�v�A��9A�%A�VA�&�A�VA��A�VA��A�~�A�n�A��-A��A�ĜA��A���A�I�A���A�|�A�;dA���A��A�`BA��RA}��Az��Aw�Ar�+Ao�TAm�mAj��Ah~�Ag�AfM�AeXAd�\A`�A[�wAZ5?AX��AUK�AShsAR(�AM��AIAH{AFZABVA<$�A9��A9�A8{A7XA77LA6��A6�jA5��A41A2��A1�wA0~�A//A.^5A.�A.A�A,�A,$�A+��A+/A*�HA+?}A+C�A*��A)x�A((�A&��A&�A$�A$��A$v�A$JA#�hA#VA"�`A"��A"��A"��A"ffA!�TA!dZA Ap�A�A�yAĜAQ�A33AM�A�mAt�A�HA~�A-A��AVA�9AM�AJA��A��AK�A��A�
AS�A�AA��A�/A��A��Az�A1'A��At�A�uA��AhsAĜA�AG�A��AQ�A^5AQ�A(�A��A�-AC�A�AĜAn�A5?A��A��AoA
��A
�+A
�DA
�DA
5?A	�-A	|�A	�A	XA	O�A	?}A	oA�jAAXA7LA"�A��A�`A��A�A�AXA�A��A��A��An�AjAbNA�A ��A @��m@���@���@���@�ƨ@��y@�%@�
=@�\)@��@�@��@�/@�I�@�b@�F@���@��T@�%@�A�@�ƨ@�C�@��@�+@땁@�
=@��@��@陚@�7L@��`@�@�A�@��;@睲@��@�ff@��@��m@�|�@��@��H@���@�\@�^5@��@�G�@�j@��u@�1@�+@ް!@ާ�@�V@�{@���@�@ݺ^@ݡ�@ݑh@ܼj@�(�@۶F@�l�@�\)@�;d@�
=@�n�@١�@�%@�r�@��
@�+@�M�@�X@���@�(�@�C�@�E�@��T@Ѳ-@љ�@�`B@���@�r�@� �@�1@�  @��;@ϝ�@�C�@��@�
=@��@θR@�~�@́@�%@��/@�z�@�(�@˕�@�+@�o@���@�ȴ@�5?@�@ɉ7@�O�@���@�(�@ǝ�@�C�@�+@��y@�ȴ@Ɨ�@�ff@�$�@Ų-@���@�  @þw@Õ�@�S�@�@�ȴ@°!@�ff@�@���@�&�@�1'@�
=@�5?@�J@��@�@�p�@�&�@���@�Q�@��m@�t�@���@�5?@��T@��@��j@�r�@�1@��F@�S�@��!@�n�@��@��@��T@�@��@�I�@��m@���@�K�@��@��\@�E�@���@�p�@�X@�V@���@�33@�~�@��7@��D@�(�@�  @��@�S�@��@�M�@��@��h@�&�@���@�Ĝ@�r�@�I�@��@�t�@��\@�-@�@��7@�O�@�/@��/@�9X@���@��@�^5@�M�@�E�@�=q@�-@��@�@��T@���@��j@�I�@�1'@�(�@��@���@�l�@�C�@�
=@��!@�J@�X@��u@�Q�@�I�@�9X@� �@�b@��
@�33@��+@��#@��h@�x�@�`B@�X@�X@�X@�?}@���@��/@��j@��@�(�@�  @���@��w@��@�dZ@�@�ȴ@���@�n�@�=q@��@��T@���@��/@���@��D@�j@�A�@��@��w@��P@�K�@�+@�"�@��@��R@�n�@�J@�/@�z�@��;@��P@�
=@�~�@�=q@���@��-@�X@��@�r�@���@��m@��w@�t�@���@�v�@�E�@�J@��#@���@��7@�x�@�p�@�`B@�O�@�V@��`@��D@�9X@� �@���@��
@���@�K�@���@���@���@��+@�ff@�-@��@��-@�`B@�/@���@��/@���@��j@��D@�A�@�S�@�ȴ@���@�n�@�M�@�{@�J@��T@���@���@���@��h@��7@�`B@��@���@��j@��9@��@~v�@}�@|�/@{��@{�@{t�@{S�@zM�@yhs@x�`@xĜ@x��@x�@xbN@x  @v�R@u�@uO�@t�@s�@r�H@rn�@rM�@rM�@r=q@r=q@r�@r�@q��@p�@p �@p  @o��@o��@o�w@o;d@nv�@m@l��@l9X@k�F@kdZ@kdZ@kC�@ko@j=q@i�7@ix�@iG�@i&�@i&�@i&�@i%@h�`@h�9@hr�@hbN@hA�@g�w@fȴ@fV@fE�@fE�@fE�@fE�@f5?@e�T@e/@d�/@dZ@d9X@ct�@bn�@bJ@a��@`��@` �@_��@^�@]�-@\�/@\��@\��@\z�@\1@[��@[dZ@[dZ@[dZ@[dZ@[dZ@[dZ@[dZ@Z�H@Y��@Yx�@Y7L@Y%@XĜ@X��@XA�@X  @W��@W�w@W�@W�@W|�@WK�@V�@Vff@U�-@U?}@T��@T1@S"�@R�\@RM�@Q�@Q��@Q��@Q��@Q�7@Qhs@Pr�@O��@O��@O\)@N�R@Nv�@M�T@M`B@MV@L�j@K�F@J�!@J��@J�\@J~�@Jn�@JM�@I�@I��@I%@H�u@H  @F�@F5?@E�-@EO�@E�@D�/@D��@Dj@C�m@B�H@B~�@B^5@BJ@@��@@Q�@?��@?�P@>�y@>�+@>ff@>ff@>E�@>5?@>@=O�@<�j@<j@<9X@;�
@;��@;�@;t�@;33@:�H@:��@:��@:n�@:J@9��@9��@9x�@9&�@81'@7�w@7l�@7�@7�@6�y@6�R@65?@5@5�@5�@4�@4�/@4��@4�D@4(�@3��@3"�@3@2~�@2^5@2M�@2J@1��@1�@1�#@1��@1X@1G�@1&�@0��@0��@0�@0 �@/�@/��@/�w@/|�@/K�@/;d@/+@/+@.��@.��@.v�@.V@.5?@.5?@.5?@.{@-�-@-p�@-O�@-O�@-?}@-/@-�@,��@,�/@,��@,j@,Z@,�@+ƨ@+dZ@+S�@+33@+o@+@*�@*��@*J@*J@)�#@)�7@)X@)�@(��@(��@(bN@(A�@(b@(  @(  @'�@'�@'K�@'�@&��@%�T@%�h@%/@%V@$��@$�@$�@$�@$�@$�j@$9X@#�
@#ƨ@#�F@#�@#"�@#o@#@"�H@"�H@"��@"��@"�!@"�!@"��@"^5@"=q@"-@!��@!�@ r�@ 1'@ b@   @�@�@�P@�P@\)@��@ff@$�@��@p�@�/@�j@Z@��@33@�@��@��@�\@n�@n�@^5@M�@�#@�^@�7@hs@7L@�G�O�Aڥ�Aڧ�Aڰ!Aک�Aڰ!Aڲ-Aڰ!Aڴ9AڮAڲ-Aڰ!AڮAڰ!Aک�AڮAڬAڮAک�AڬAڬAک�Aڰ!AڮAڰ!Aڰ!Aڲ-Aڴ9Aڰ!Aڴ9Aڰ!Aڲ-Aڰ!Aڰ!Aڲ-AڮAڰ!AڬAڮAڰ!AڬAڲ-AڬAڰ!AڮAڮAڲ-AڬAڰ!AڬAڬAڬAک�AڮAڥ�AڮAڬAڥ�Aڣ�Aڥ�Aڣ�Aڴ9Aڲ-AڶFAڴ9Aڴ9AڸRAڴ9Aڴ9AڸRAڴ9AڶFAڶFAڲ-AڮAڲ-AڮAڮAڲ-Aڛ�Aڥ�AڮG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
XyB
XyB
X�B
X�B
XyB
XyB
XyB
X�B
XyB
XyB
XyB
XyB
XyB
XEB
XyB
XyB
W�B
XyB
XEB
YKB
YB
[�B
]�B
bB
c�B
d�B
ffB
h�B
hsB
jB
j�B
jB
jKB
iB
g�B
g�B
iB
iB
h�B
g�B
f2B
c B
_;B
P�B
7�B
�B	�"B	�iB	�B	�PB
B
4�B
a�B
�B
��B
��B
�]B�BG�Be�Bg�BiBiDBkQBi�BgmBc BaHB_�Be`Bc�Bh�Bc�Bo Be�B^5B\�BXBS�BUgBGEBJ�B-�B)�B#BB$B
��B
�|B
��B
��B
�VB
��B
|�B
y�B
qvB
o5B
^�B
V9B
0�B
4B	�xB	�B	��B	�[B	�zB	�_B	�VB	��B	��B	��B	��B	z�B	e,B	^5B	Z�B	M�B	@�B	8B	1�B	!�B	=B	�B	�B	B	 B	�B	�B	B	�B	=B	B	 �B	/�B	9�B	C-B	K�B	T�B	V9B	a�B	{�B	�DB	�JB	��B	��B	��B	�B	��B	�OB	��B	�^B	��B	�B	��B	ɆB	͟B	�,B	��B	�&B	��B	�WB	��B	�B	�B	�B	�GB	��B	��B	�VB	��B	�]B
{B
DB
(B
�B
B
�B
B
B
�B
�B
FB
�B
1B
VB
 �B
(�B
.IB
/OB
,qB
+B
+B
,=B
0!B
49B
6zB
6�B
6B
1�B
0UB
0UB
2�B
2�B
/�B
,qB
+kB
.IB
+�B
,�B
3�B
3�B
2�B
0UB
/OB
1[B
1�B
2�B
1�B
1�B
49B
6�B
:^B
:�B
=qB
=�B
?}B
<6B
=�B
FtB
E�B
E9B
EB
C�B
C-B
?�B
=qB
<�B
<6B
;�B
:*B
9$B
6FB
1[B
0!B
-�B
*�B
'B
 �B
B
B
CB
�B
{B
�B
�B
uB
�B
�B
�B
B
 4B	��B	�B	��B	��B	�]B	��B	�B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�B
 iB
MB
B
�B
YB
�B
�B

	B
�B
B
DB
xB
JB
B
xB
�B
DB

�B

�B

=B

	B

	B
	7B
1B
�B
1B
+B
_B
YB
�B
%B
�B
%B
�B
�B
SB
%B
SB
�B
SB
�B
MB
�B
�B
MB
�B
MB
�B
B
GB
B
{B
GB
�B
�B
MB
B
{B
�B
B
{B
�B
{B
GB
�B
B
B
{B
B
B
GB
GB
�B
�B
MB
B
�B
B
�B
B
MB
�B
B
�B
�B
�B
�B
�B
%B
�B
�B
YB
YB
�B
�B
%B
YB
1B
+B
�B
+B
+B
_B
�B
�B
�B
�B
�B
�B
fB
�B
�B
�B
�B
�B
�B
	B
�B
	B
�B
fB
	�B
fB
�B
	�B
	lB
	lB

	B
	�B

rB
DB
xB
�B
xB
DB
DB
DB
B
�B
B
PB
�B
�B
�B
�B
VB
�B
�B
�B
VB
�B
�B
hB
4B
4B
 B
hB
�B
B
:B
�B
�B
�B
�B
�B
�B
uB
B
FB
FB
{B
�B
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
�B
�B
1B
_B
�B
�B
�B
�B
7B
7B
�B
�B
�B
�B
�B
qB
CB
�B
�B
xB
CB
xB
B
CB
IB
�B
�B
!B
!B
!B
�B
!B
�B
�B
VB
!B
�B
�B
 'B
�B
 \B
 \B
 \B
 �B
 �B
 �B
!-B
!-B
!�B
!-B
 �B
 �B
!�B
!-B
 �B
!bB
!bB
"�B
"4B
"�B
#nB
#nB
#:B
#nB
#�B
$B
$B
$�B
%�B
&LB
&�B
'�B
(XB
(XB
(�B
(XB
(�B
($B
)�B
*eB
)�B
*0B
*0B
+6B
+�B
+kB
,=B
,B
,�B
,qB
,�B
,�B
,�B
,�B
-wB
-B
.B
.IB
.�B
/B
/OB
/�B
0UB
0�B
0�B
0�B
0�B
0�B
1'B
0�B
1�B
1�B
2-B
2aB
2aB
2-B
1�B
1�B
1�B
4�B
4nB
4�B
5B
4�B
5B
5?B
5tB
5�B
5�B
5�B
5�B
5�B
5�B
6B
6B
5�B
5?B
8RB
8�B
8�B
8�B
:*B
:*B
9�B
9�B
:�B
;�B
;�B
;�B
;�B
;�B
;0B
;dB
=qB
=<B
=<B
=qB
>�B
?B
?HB
?B
?B
>�B
?B
>�B
>�B
?B
@�B
@OB
@OB
@OB
?�B
@B
@OB
@�B
AUB
A�B
B[B
B[B
B�B
B�B
B�B
B[B
C�B
C�B
C�B
D3B
D3B
C�B
C�B
D3B
D3B
DgB
D�B
DgB
DgB
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F?B
FtB
FtB
FtB
GEB
G�B
G�B
G�B
HB
HKB
HKB
IRB
I�B
I�B
I�B
I�B
J#B
JXB
JXB
J�B
J�B
JXB
J�B
JXB
J#B
I�B
J�B
K^B
K^B
K^B
K^B
K�B
K�B
K�B
L0B
LdB
LdB
LdB
L0B
LdB
LdB
L�B
L�B
M�B
M�B
NB
NpB
OBB
OvB
OvB
PB
O�B
PB
O�B
O�B
O�B
QB
QNB
QNB
Q�B
R�B
R�B
R�B
S&B
S�B
R�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UgB
UgB
VmB
VB
W
B
XB
XEB
X�B
X�B
X�B
X�B
YB
YB
Y�B
Z�B
Z�B
ZQB
Z�B
\)B
[�B
\]B
\]B
]dB
]�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
_B
_;B
_�B
_�B
_�B
`BB
`BB
`B
_�B
_�B
_�B
`B
`B
`B
`B
`BB
bB
bB
b�B
b�B
b�B
b�B
b�B
c�B
c�B
d&B
d&B
d�B
d�B
d�B
d�B
e,B
e�B
e�B
e�B
f�B
ffB
f�B
g8B
f�B
f�B
f�B
g8B
gmB
g�B
gmB
g�B
g�B
hsB
hsB
h�B
h�B
h�B
iB
iyB
iB
iyB
iB
i�B
i�B
i�B
jB
jKB
jB
jB
jB
jKB
j�B
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
l"B
l�B
lWB
l"B
l�B
lWB
l"B
lWB
l�B
m]B
m)B
m�B
m�B
m�B
n/B
n/B
ncB
n�B
n�B
o B
n�B
n�B
n�B
o B
o5B
o B
pB
pB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
rB
r�B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
r�B
r�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
v+B
v+B
v+B
v`B
v�B
w2B
wfB
w�B
x�B
x�B
y>B
y>B
y>B
y>B
y>B
yrB
y>B
y>B
zB
y�B
zB
zxB
zxB
z�B
z�B
X�B
X�B
V9B
YB
X�B
XB
X�B
W�B
YKB
W�B
X�B
X�B
XEB
Z�B
W�B
X�B
XB
YB
XEB
XEB
Y�B
W
B
YB
W�B
XEB
X�B
XB
YB
W�B
YB
XyB
X�B
X�B
XB
YKB
XB
YKB
X�B
XB
Y�B
WsB
YKB
XB
X�B
XyB
XB
X�B
X�B
XyB
XyB
WsB
YB
W?B
Z�B
X�B
WsB
ZQB
YB
YB
Z�B
XB
YB
W�B
X�B
X�B
W�B
XyB
X�B
WsB
X�B
XyB
W�B
YKB
Y�B
WsB
X�B
XEB
XB
[�B
V9B
T�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         B
XsB
X�B
X�B
X�B
XbB
X�B
X�B
X�B
XxB
X�B
X�B
X�B
XEB
XeB
X�B
XhB
X;B
X�B
XyB
Y�B
Y�B
\%B
]�B
bB
c�B
d�B
fRB
h�B
hcB
jB
j�B
j�B
j|B
iSB
g�B
g�B
iB
i6B
h�B
g�B
f�B
d B
aeB
U�B
H�B
$�B	�+B	�B	�UB	��B
�B
9 B
f7B
�yB
�IB
��B
��B$�BeQBm�Bl�Bl�Bn�Bp?Bm�BixBgBh�BnBo�Bk�Bu�Bx�B�pBl�Bi�Ba�B\�B^/B]sBU@B\�B1�B,eB&�B*B!�B]B-ByB
ѿB
��B
��B
B
{qB
s�B
uMB
kPB
mCB
C�B
vB
fB	��B	ܿB	ǸB	��B	��B	��B	�6B	�"B	�
B	�yB	��B	i�B	bZB	c�B	R�B	DCB	CCB	>4B	$�B	 1B	"�B	$�B	�B	�B	oB	�B	sB	OB	B	�B	%�B	3IB	<yB	F�B	O%B	V�B	VB	b�B	[B	�PB	��B	��B	�wB	�@B	��B	��B	�B	īB	��B	�B	�>B	��B	��B	�9B	�B	ߏB	��B	�+B	�B	�uB	�B	�B	��B	�.B	��B	��B	�B	�CB
 \B
�B
xB
�B
[B
B
(B
nB
@B
�B
B
�B
{B
B
 B
"B
*�B
1�B
1"B
-DB
+rB
+@B
,�B
0�B
4\B
7(B
8	B
7�B
3WB
3�B
38B
4>B
5B
2[B
/�B
-UB
/�B
+�B
-1B
4$B
4SB
3�B
1�B
0�B
2B
2�B
3zB
2�B
3�B
6wB
8�B
:�B
;B
=�B
?2B
A�B
=
B
>B
F�B
E�B
E�B
E�B
E�B
GB
A�B
>B
=?B
<�B
<`B
;�B
<B
7�B
2UB
1QB
/VB
.�B
+�B
"�B
NB
sB
B
!B
�B
oB
JB
�B
�B
�B
	6B
oB
�B
B	�8B	��B	�B	�B	��B	�^B	��B	�B	��B	�B	�bB	�6B	�*B	�6B	�B	�eB
�B
 B
�B
ZB
1B
�B
�B

�B
�B
�B
�B
:B
 B
�B
�B
�B
�B
?B
'B

�B
AB
�B

pB
�B
	LB

B
DB
�B
*B
�B
�B
�B
IB
	B
2B
_B
�B
rB
�B
�B
�B
B
�B
�B
�B
lB
�B
�B
4B
�B
�B
8B
�B
�B
�B
�B
mB
-B
AB
�B
?B
$B
�B
�B
`B
�B
�B
�B
fB
�B
&B
�B
B
<B
FB
B
�B

B
6B
sB
�B
B
-B
$B
,B
�B
�B
�B
�B
B
+B
�B
�B
B
WB
�B
�B
	�B
�B
KB
�B
�B
�B
�B
B
�B
�B
�B
	�B

YB

B
	"B
�B
	B
	^B
	WB
	�B
	�B
	�B
	�B
	�B

lB
	B
	�B

EB
	�B

+B

�B

�B
�B
�B
AB
�B
�B
�B
�B
>B
�B
>B
�B
�B
aB
B
�B
�B
�B
�B
�B
�B
�B
BB
xB
B
�B
�B
�B
lB
�B
�B
B
tB
B
JB
JB
?B
�B
�B
�B
B
�B
oB
_B
@B
BB
<B
SB
�B
�B
%B
B
�B
�B
&B
8B
�B
B
�B
�B
B
�B
�B
�B
<B
3B
(B
oB
�B
�B
�B
/B
�B
�B
|B
�B
�B
�B
�B
�B
�B
\B
WB
6B
�B
+B
-B
sB
�B
uB
 B
 =B
 �B
 ZB
 �B
 �B
!B
!�B
!~B
!oB
!�B
!�B
!�B
!�B
!�B
"&B
"B
!pB
!NB
!�B
"B
#B
"�B
#bB
#�B
#�B
#�B
#�B
$WB
%B
%�B
&vB
'B
'B
'�B
(�B
(�B
);B
(�B
)+B
)pB
)�B
*�B
*�B
*kB
*�B
+MB
,KB
,B
+�B
,�B
,xB
,�B
,�B
,�B
,�B
,�B
-pB
-�B
-�B
.�B
.�B
/B
/sB
/�B
0FB
1B
1B
0�B
0�B
0�B
1B
1�B
1�B
2AB
2B
2�B
2�B
2�B
2iB
2�B
2�B
3�B
5�B
4�B
5(B
5gB
5VB
53B
5�B
5�B
6B
5�B
5�B
5�B
6B
6�B
6�B
6VB
6AB
7KB
9�B
9�B
9�B
9�B
:�B
:NB
:B
:�B
;�B
<^B
;�B
;�B
;�B
;�B
;�B
<�B
>�B
=�B
>B
>�B
?qB
?�B
?qB
?B
?*B
>�B
?:B
>�B
?9B
@xB
@�B
@B
@�B
@ZB
@B
@�B
A:B
A�B
BvB
B�B
B�B
B�B
B�B
B�B
CB
CNB
D�B
C�B
DB
DYB
D9B
D	B
D*B
DYB
DfB
D�B
D�B
D�B
D�B
E�B
F>B
E�B
E�B
E�B
E�B
E�B
E�B
F{B
F�B
F�B
F�B
G3B
H-B
HB
H%B
H�B
H�B
H�B
IB
JdB
JJB
J0B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J^B
J�B
J^B
J3B
J|B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L;B
L`B
LxB
LwB
LlB
LcB
L�B
L�B
MAB
M|B
NIB
NEB
N�B
OjB
O�B
O�B
O�B
PWB
O�B
P)B
O�B
PB
P�B
Q�B
Q�B
Q�B
R4B
SB
S+B
S�B
S�B
TB
TB
U�B
UB
T�B
T�B
T�B
T�B
U<B
U�B
VB
V�B
V�B
X9B
X�B
X�B
YJB
YB
Y/B
Y/B
Y^B
Y�B
Z�B
[&B
Z�B
Z�B
[�B
\�B
\�B
\�B
]B
]�B
]�B
]�B
]�B
]�B
]�B
^�B
_<B
^�B
_JB
_�B
`B
`B
_�B
`�B
`�B
`6B
`B
`B
`B
`]B
`@B
`VB
`�B
aeB
b�B
b�B
b�B
b�B
b�B
cB
c�B
d@B
c�B
d�B
ddB
d�B
eB
e!B
ezB
e�B
f,B
fB
f�B
f�B
f�B
f�B
gNB
f�B
f�B
gB
g�B
g�B
g�B
g�B
g�B
hDB
h�B
h�B
h�B
h�B
h�B
iLB
i�B
i+B
i�B
iZB
jB
i�B
jB
j>B
jPB
j"B
jLB
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k9B
k�B
k�B
lB
l�B
l�B
lvB
lQB
l�B
lqB
lHB
l�B
m�B
mmB
mmB
m�B
nB
nIB
ndB
n�B
n�B
n�B
oB
oB
n�B
n�B
o'B
ouB
o�B
o�B
p�B
ptB
qB
qB
p�B
p�B
p�B
p�B
p�B
q%B
quB
rB
q�B
q�B
r&B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s B
sDB
sB
spB
tZB
t�B
u
B
t�B
t�B
t�B
t�B
uB
t�B
uB
u�B
v�B
v�B
v�B
v�B
w�B
wqB
w�B
x�B
yMB
x�B
y{B
yhB
yXB
ydB
yHB
y�B
yfB
y�B
z=B
zB
z?B
z�B
z�B
z�G�O�B
X�B
X�B
V9B
YB
X�B
XB
X�B
W�B
YKB
W�B
X�B
X�B
XEB
Z�B
W�B
X�B
XB
YB
XEB
XEB
Y�B
W
B
YB
W�B
XEB
X�B
XB
YB
W�B
YB
XyB
X�B
X�B
XB
YKB
XB
YKB
X�B
XB
Y�B
WsB
YKB
XB
X�B
XyB
XB
X�B
X�B
XyB
XyB
WsB
YB
W?B
Z�B
X�B
WsB
ZQB
YB
YB
Z�B
XB
YB
W�B
X�B
X�B
W�B
XyB
X�B
WsB
X�B
XyB
W�B
YKB
Y�B
WsB
X�B
XEB
XB
[�B
V9B
T�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�ec<��(<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<r�M<�h�<��v<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<r<20�<#�
<Z�"<���<�Q<#�
<D+�<#�
<#�
<3x�<#�
<lM<�ޝ<#�
<#�
<#�
<[��<7+<#�
<v�e<�N�<�n�<+d<#�
<#�
<#�
<#�
<#�
<X�]<�v�<��<4�</PQ<��q<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<+ .<Y�<#�
<#�
<#�
<#�
<#�
<@�%<W-�<#�
<#�
<?r�<��g<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021080905330520210809053305IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021081907010620210819070106QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021081907010620210819070106QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364820220422133648IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064520220426160645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064520220426160645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064520220426160645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                
CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-04-04T01:24:49Z creation; 2023-05-01T21:35:41Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     x  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  d`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʨ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ҈   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20210404012449  20230501213541  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               w   wAA  AOAO7314_008642_119                 7314_008642_119                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�j��{��@�j��{��11  @�j�����@�j�����@1�jOv@1�jOv�b�k�v���b�k�v��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@B�\@�G�@�G�@\@�G�A   A��A ��A,(�A?\)A`  A�  A�\)A�\)A��A�  A�  A�  A�\)A��B�
B  B  B�
B((�B/�
B7�
B@  BG�
BO�BX  B`  Bg�
Bp  Bw�B�
B�  B�{B�(�B�  B��B�  B�{B�{B�{B�  B�  B��B�  B�  B��B�  B��B��B�  B�  B�{B�{B�  B�{B��B��
B�  B�{B�  B�  B�  C   C
=C��C��C��C
  C
=C  C  C��C��C��C
=C{C  C��C��C"  C$  C%��C(  C)��C+�C-��C/��C2  C4  C6  C8
=C:
=C<
=C>  C@  CB
=CC��CF  CH
=CJ  CL  CN  CP  CR
=CT
=CV  CW��CY��C\
=C^  C`
=Cb
=Cd
=Cf
=Ch  Ci��Ck��Cm��Cp  Cr
=Ct
=Cv
=Cx  Cz
=C|
=C}��C��C���C�  C�  C�  C���C���C�C�  C���C���C�  C�C�  C�C�  C�  C�C���C���C���C�C�C�
=C�  C���C�C�C�  C�  C�  C�C�  C���C�C�
=C�C���C�  C�C�C�
=C�  C�  C�  C�C�C���C�  C�  C���C���C�  C���C�  C�  C���C���C���C���C���C���C���C�  C�  C�  C�  C�C�C�C���C�C�  C���C�C�C�C�C���C���C�
=C�
=C�C�  C���C�  C�C�  C���C�  C�C�C�  C���C�  C�  C���C���C���C���C�  C�\C�
=C�C�
=C�C�C�  C�  C�  C�  C�C�C�C�C�C�
=C�
=C�\C�C���C�  C�C�  C�  C�C���C�  D   D ��D  Dz�D�qD� D  D��DD� D�qD� D�D�D�D}qD  D��D�qD	z�D	�qD
� D�D�D  Dz�D�qD� D�D� D��D}qD�D� D�qD��D�D��DD��D  D��D  D}qD  D��D  D}qD�D� D�qD��D  D� D  Dz�D  D��D�qDz�D  D}qD�qD}qD��D � D!  D!� D"  D"� D"�qD#}qD$  D$� D%  D%��D&D&��D&�qD'� D(�D(}qD(��D)}qD*�D*��D+�D+� D,  D,}qD,�qD-}qD-�qD.}qD.�qD/� D0  D0��D1D1��D1�qD2��D3  D3}qD4  D4��D5�D5}qD5��D6� D7�D7��D8�D8� D8�qD9� D:  D:}qD;  D;}qD<  D<� D=  D=� D=�qD>}qD>��D?� D@�D@��DA�DA� DB  DB� DB�qDC��DD  DD}qDD�qDE}qDE�qDF� DG  DG}qDG�qDH}qDI  DI��DJ  DJ}qDJ�qDK� DK�qDL� DM�DM� DN  DN}qDO  DO��DP�DP� DQ  DQ� DR  DR��DS  DS}qDT  DT��DU  DU� DV�DV� DV��DW� DX  DX� DY�DY}qDZ  DZ��D[�D[� D\  D\}qD]�D]� D^  D^� D_  D_� D_�qD`}qD`�qDa� Db�Db� Db�qDc}qDc�qDd� De�De� Df�Df��Dg�Dg� Dg�qDh� Di�Di� Di�qDj� Dk  Dk}qDk�qDl}qDl�qDm}qDm�qDn� Do�Do��Dp�Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du��DvDv��Dw  Dwz�Dw�qDx� Dy  Dy� Dz�Dz�D{�D{��D|  D|}qD|��D}� D~D~� D~��D}qD�qD�@ D�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�>�D�� D��HD�HD�>�D�~�D�� D�  D�@ D���D�D�  D�@ D�� D�� D�  D�@ D��HD���D�  D�@ D�~�D�� D�  D�>�D�}qD���D�HD�@ D�~�D��HD�  D�=qD�~�D��HD�  D�>�D�~�D���D���D�@ D�� D��HD���D�=qD�� D��HD���D�=qD�~�D�� D���D�>�D��HD�� D�  D�>�D�~�D���D���D�>�D�}qD�� D��D�B�D�� D�� D�  D�=qD�}qD��HD�  D�>�D��HD�� D��qD�@ D��HD�� D���D�>�D�~�D�� D�HD�>�D�~�D�� D��D�AHD�~�D���D�  D�@ D�� D�D�HD�@ D�� D��HD��D�@ D�� D�� D��qD�>�D�� D��HD���D�=qD�~�D��HD���D�@ D���D�� D��D�@ D�~�D��HD�  D�=qD�~�D�� D��D�@ D�� D��HD�  D�>�D�� D���D���D�@ D�~�D���D�HD�@ D�~�D�� D���D�=qD�� D�D�HD�@ D�~�D���D�  D�>�D�}qD�� D���D�=qD�~�D�� D�HD�@ D�~�D���D�HD�>�D�}qD�� D�HD�@ D�~�D�� D�HD�B�D��HD���D�  D�@ D���D��HD�  D�@ D�~�D�� D�HD�B�D��HD���D���D�@ D�� D���D��qD�@ D���D��HD�  D�AHD�� D���D���D�@ D���D�� D���D�@ D�� D���D�  D�AHD���D��HD���D�@ D��HD��HD�HD�B�D��HD��HD��D�AHD���D��HD�  D�@ D�� D���D���D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�D��HD�HD�@ D��HD��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D���D�>�DÀ D�� D�  D�@ D�~�Dľ�D�  D�@ DŁHD��HD�HD�AHDƁHDƾ�D�  D�B�DǁHD��HD�  D�>�DȀ D�� D�  D�@ Dɀ D��HD���D�@ DʁHD�� D�  D�AHDˀ D�� D�HD�@ D̀ D̾�D�HD�@ D́HD�D�HD�@ D�~�D�� D�  D�@ Dπ DϾ�D���D�>�D�~�D�� D�HD�@ Dр D��HD�HD�@ DҀ D��HD���D�@ DӁHD�� D�  D�>�D�~�DԾ�D���D�>�DՀ Dվ�D��qD�>�DցHD�� D���D�@ DׁHD��HD�  D�@ D�~�Dؾ�D���D�@ Dـ D�� D�  D�AHDڀ D�� D�  D�B�D�~�D۾�D�HD�AHD܁HD��HD��D�AHD݀ D��HD�HD�B�Dނ�D޾�D���D�AHD߁HD�� D�  D�=qD�}qD�� D�HD�>�D�~�DᾸD�HD�@ D�~�D⾸D�  D�AHD� D�� D���D�>�D� D��HD�  D�>�D�HD�D�HD�AHD� D�� D���D�>�D�~�D�qD��qD�@ D肏D��HD���D�>�D� D龸D��qD�>�D� D�� D�  D�AHD� D뾸D�  D�@ D�~�D�� D���D�=qD�~�D�� D���D�=qD�~�D��HD�  D�>�D�~�D�� D�  D�AHD�� D�D�  D�@ D�~�D�D�  D�AHD� D�D�HD�@ D� D�� D�  D�@ D�~�D��HD�  D�>�D��HD��HD�  D�@ D�� D�� D�  D�AHD�� D���D���D�AHD�� D�� D�HD�B�D���D�D��{D�7
?\)?aG�?�\)?�33?��?�@��@�R@.{@B�\@Tz�@fff@xQ�@��@�\)@�Q�@�G�@�=q@�33@�p�@�ff@�\)@ٙ�@��
@���@�
=A   A�A��A{A33A�A��A!�A&ffA,(�A0��A5A:�HA@  ADz�AI��AMp�AQ�AW
=A\(�A`��AeAj=qAo\)Atz�Ax��A~�RA��A�(�A��RA���A��
A�{A���A�33A�A�Q�A��\A���A�\)A��A�(�A�
=A�G�A��A�{A�Q�A��HA��A��A�=qA���A�\)A��A�z�A�
=Aə�A��
A�{AУ�A��HA��A�  Aڏ\A��A߮A�=qA�z�A�RA�G�A�A�{A�Q�A��HA�p�A�  A��\A���A�\)B�B=qB�B��B�B33BQ�B	��B
=BQ�Bp�B�RB�
B�BffB�B��B{B\)B��B�B
=B(�Bp�B�\B�
B ��B"ffB#�B$��B&=qB'�B(��B)�B+33B,Q�B-p�B.�HB0(�B1p�B2�HB4(�B5p�B6�\B7�B8��B:=qB;�B<��B>{B?\)B@��BB{BC33BDz�BE��BF�RBH(�BI��BJ�HBL(�BMp�BNffBO�BP��BR=qBS\)BT��BV=qBW�BX��BY�B[
=B\Q�B]B^�HB`Q�Ba��Bb�HBd(�BeG�Bf�\Bg�
Bi�BjffBk�Bm�BnffBo�Bp��Br=qBs\)Bt��Bu�Bw\)Bx��Bz{B{\)B|��B}�B
=B�=qB��HB���B�Q�B��HB��B�(�B���B��B�=qB��HB��B�(�B��RB�\)B�  B���B�\)B�  B��RB�\)B�  B��\B�33B��
B�z�B�33B��
B��\B�33B�B�Q�B���B��B�Q�B�
=B�B�ffB���B���B�=qB��HB��B�=qB��HB���B�=qB��HB�p�B�{B��RB�G�B�  B���B�\)B�  B��\B�33B��
B�ffB�
=B��B�Q�B�
=B���B�(�B���B�\)B�  B���B�G�B��B�z�B�
=B���B�(�B���B�p�B�{B��RB�G�B��
B�ffB���B���B�(�B���B�p�B�  B��\B�
=B�B�ffB�
=B��B�(�B��RB�p�B�{B���B�p�B�  B�z�B�33B��
B\B�33B��
B�ffB�
=Bř�B�Q�B�
=BǮB�Q�B��HB�p�B�(�B��HB˅B�(�B̸RB�\)B�  BθRB�\)B�{BУ�B�G�B��Bң�B�G�B��Bԏ\B�33B��B֏\B�G�B��B؏\B��B��
Bڏ\B�33B��B�z�B��B�Bޏ\B�G�B�  B��\B�33B��B��B�\)B�  B�\B�G�B�  B�RB�\)B�  B��B�p�B�{B��HB�B�=qB���B홚B�Q�B�
=B�B�ffB��B��
B��B�\)B�  B���B�p�B�(�B���B��B�Q�B�
=B��
B��\B�G�B�  B���B�p�B�=qB���B��C (�C �C �CQ�C�C  CffC��C(�C�C�HC33C��C
=CffC�RC(�C�\C��CQ�C��C{Cz�C�
C	33C	��C
  C
ffC
C(�C�\C��CQ�C�C{C�C�CG�C��C{C�C�HCG�C�RC{Cz�C�
C=qC�C
=CffC�
C=qC�C
=Cp�C�
CG�C��C
=Cz�C�HCG�C��C  CffC��C{CffCC{C\)C��C�HC33Cp�C��C�HC�C\)C�C��C
=C=qCp�C�RC��C�C\)C��C�HC {C G�C �\C ��C!
=C!=qC!p�C!�C!��C"33C"\)C"��C"�
C#{C#Q�C#�C#C$  C$=qC$p�C$��C$�C%(�C%ffC%��C%�HC&�C&Q�C&�C&��C'
=C'=qC'�C'C(  C(33C(p�C(�RC)  C)33C)p�C)��C)��C*(�C*ffC*��C*�HC+�C+Q�C+��C+�
C,
=C,=qC,�C,C-  C-33C-z�C-�RC-��C.(�C.ffC.�C.�C/�C/\)C/��C/�HC0{C0Q�C0�\C0C1
=C1G�C1p�C1�C1��C2(�C2\)C2�\C2�
C3{C3=qC3�C3C3��C4(�C4ffC4�C4�
C5{C5\)C5�\C5�RC6  C6G�C6p�C6�C6��C733C7ffC7��C7�C8{C8Q�C8�\C8C8��C9G�C9z�C9��C9�C:(�C:\)C:��C:�HC;
=C;G�C;�\C;��C<  C<33C<p�C<�C<�HC=(�C=ffC=��C=��C>�C>G�C>�C>��C?
=C?=qC?p�C?�RC?��C@(�C@p�C@��C@�HCA{CA\)CA�\CA��CB{CBG�CBp�CB�RCB��CC(�CCffCC��CC�
CD{CD\)CD�CDCE
=CEG�CEp�CE�RCE��CF�CFffCF�CF�
CG{CG\)CG�CG��CH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                       ?�  ?��H@B�\@�G�@�G�@\@�G�A   A��A ��A,(�A?\)A`  A�  A�\)A�\)A��A�  A�  A�  A�\)A��B�
B  B  B�
B((�B/�
B7�
B@  BG�
BO�BX  B`  Bg�
Bp  Bw�B�
B�  B�{B�(�B�  B��B�  B�{B�{B�{B�  B�  B��B�  B�  B��B�  B��B��B�  B�  B�{B�{B�  B�{B��B��
B�  B�{B�  B�  B�  C   C
=C��C��C��C
  C
=C  C  C��C��C��C
=C{C  C��C��C"  C$  C%��C(  C)��C+�C-��C/��C2  C4  C6  C8
=C:
=C<
=C>  C@  CB
=CC��CF  CH
=CJ  CL  CN  CP  CR
=CT
=CV  CW��CY��C\
=C^  C`
=Cb
=Cd
=Cf
=Ch  Ci��Ck��Cm��Cp  Cr
=Ct
=Cv
=Cx  Cz
=C|
=C}��C��C���C�  C�  C�  C���C���C�C�  C���C���C�  C�C�  C�C�  C�  C�C���C���C���C�C�C�
=C�  C���C�C�C�  C�  C�  C�C�  C���C�C�
=C�C���C�  C�C�C�
=C�  C�  C�  C�C�C���C�  C�  C���C���C�  C���C�  C�  C���C���C���C���C���C���C���C�  C�  C�  C�  C�C�C�C���C�C�  C���C�C�C�C�C���C���C�
=C�
=C�C�  C���C�  C�C�  C���C�  C�C�C�  C���C�  C�  C���C���C���C���C�  C�\C�
=C�C�
=C�C�C�  C�  C�  C�  C�C�C�C�C�C�
=C�
=C�\C�C���C�  C�C�  C�  C�C���C�  D   D ��D  Dz�D�qD� D  D��DD� D�qD� D�D�D�D}qD  D��D�qD	z�D	�qD
� D�D�D  Dz�D�qD� D�D� D��D}qD�D� D�qD��D�D��DD��D  D��D  D}qD  D��D  D}qD�D� D�qD��D  D� D  Dz�D  D��D�qDz�D  D}qD�qD}qD��D � D!  D!� D"  D"� D"�qD#}qD$  D$� D%  D%��D&D&��D&�qD'� D(�D(}qD(��D)}qD*�D*��D+�D+� D,  D,}qD,�qD-}qD-�qD.}qD.�qD/� D0  D0��D1D1��D1�qD2��D3  D3}qD4  D4��D5�D5}qD5��D6� D7�D7��D8�D8� D8�qD9� D:  D:}qD;  D;}qD<  D<� D=  D=� D=�qD>}qD>��D?� D@�D@��DA�DA� DB  DB� DB�qDC��DD  DD}qDD�qDE}qDE�qDF� DG  DG}qDG�qDH}qDI  DI��DJ  DJ}qDJ�qDK� DK�qDL� DM�DM� DN  DN}qDO  DO��DP�DP� DQ  DQ� DR  DR��DS  DS}qDT  DT��DU  DU� DV�DV� DV��DW� DX  DX� DY�DY}qDZ  DZ��D[�D[� D\  D\}qD]�D]� D^  D^� D_  D_� D_�qD`}qD`�qDa� Db�Db� Db�qDc}qDc�qDd� De�De� Df�Df��Dg�Dg� Dg�qDh� Di�Di� Di�qDj� Dk  Dk}qDk�qDl}qDl�qDm}qDm�qDn� Do�Do��Dp�Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du��DvDv��Dw  Dwz�Dw�qDx� Dy  Dy� Dz�Dz�D{�D{��D|  D|}qD|��D}� D~D~� D~��D}qD�qD�@ D�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�>�D�� D��HD�HD�>�D�~�D�� D�  D�@ D���D�D�  D�@ D�� D�� D�  D�@ D��HD���D�  D�@ D�~�D�� D�  D�>�D�}qD���D�HD�@ D�~�D��HD�  D�=qD�~�D��HD�  D�>�D�~�D���D���D�@ D�� D��HD���D�=qD�� D��HD���D�=qD�~�D�� D���D�>�D��HD�� D�  D�>�D�~�D���D���D�>�D�}qD�� D��D�B�D�� D�� D�  D�=qD�}qD��HD�  D�>�D��HD�� D��qD�@ D��HD�� D���D�>�D�~�D�� D�HD�>�D�~�D�� D��D�AHD�~�D���D�  D�@ D�� D�D�HD�@ D�� D��HD��D�@ D�� D�� D��qD�>�D�� D��HD���D�=qD�~�D��HD���D�@ D���D�� D��D�@ D�~�D��HD�  D�=qD�~�D�� D��D�@ D�� D��HD�  D�>�D�� D���D���D�@ D�~�D���D�HD�@ D�~�D�� D���D�=qD�� D�D�HD�@ D�~�D���D�  D�>�D�}qD�� D���D�=qD�~�D�� D�HD�@ D�~�D���D�HD�>�D�}qD�� D�HD�@ D�~�D�� D�HD�B�D��HD���D�  D�@ D���D��HD�  D�@ D�~�D�� D�HD�B�D��HD���D���D�@ D�� D���D��qD�@ D���D��HD�  D�AHD�� D���D���D�@ D���D�� D���D�@ D�� D���D�  D�AHD���D��HD���D�@ D��HD��HD�HD�B�D��HD��HD��D�AHD���D��HD�  D�@ D�� D���D���D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�D��HD�HD�@ D��HD��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D���D�>�DÀ D�� D�  D�@ D�~�Dľ�D�  D�@ DŁHD��HD�HD�AHDƁHDƾ�D�  D�B�DǁHD��HD�  D�>�DȀ D�� D�  D�@ Dɀ D��HD���D�@ DʁHD�� D�  D�AHDˀ D�� D�HD�@ D̀ D̾�D�HD�@ D́HD�D�HD�@ D�~�D�� D�  D�@ Dπ DϾ�D���D�>�D�~�D�� D�HD�@ Dр D��HD�HD�@ DҀ D��HD���D�@ DӁHD�� D�  D�>�D�~�DԾ�D���D�>�DՀ Dվ�D��qD�>�DցHD�� D���D�@ DׁHD��HD�  D�@ D�~�Dؾ�D���D�@ Dـ D�� D�  D�AHDڀ D�� D�  D�B�D�~�D۾�D�HD�AHD܁HD��HD��D�AHD݀ D��HD�HD�B�Dނ�D޾�D���D�AHD߁HD�� D�  D�=qD�}qD�� D�HD�>�D�~�DᾸD�HD�@ D�~�D⾸D�  D�AHD� D�� D���D�>�D� D��HD�  D�>�D�HD�D�HD�AHD� D�� D���D�>�D�~�D�qD��qD�@ D肏D��HD���D�>�D� D龸D��qD�>�D� D�� D�  D�AHD� D뾸D�  D�@ D�~�D�� D���D�=qD�~�D�� D���D�=qD�~�D��HD�  D�>�D�~�D�� D�  D�AHD�� D�D�  D�@ D�~�D�D�  D�AHD� D�D�HD�@ D� D�� D�  D�@ D�~�D��HD�  D�>�D��HD��HD�  D�@ D�� D�� D�  D�AHD�� D���D���D�AHD�� D�� D�HD�B�D���D�D��{G�O�?\)?aG�?�\)?�33?��?�@��@�R@.{@B�\@Tz�@fff@xQ�@��@�\)@�Q�@�G�@�=q@�33@�p�@�ff@�\)@ٙ�@��
@���@�
=A   A�A��A{A33A�A��A!�A&ffA,(�A0��A5A:�HA@  ADz�AI��AMp�AQ�AW
=A\(�A`��AeAj=qAo\)Atz�Ax��A~�RA��A�(�A��RA���A��
A�{A���A�33A�A�Q�A��\A���A�\)A��A�(�A�
=A�G�A��A�{A�Q�A��HA��A��A�=qA���A�\)A��A�z�A�
=Aə�A��
A�{AУ�A��HA��A�  Aڏ\A��A߮A�=qA�z�A�RA�G�A�A�{A�Q�A��HA�p�A�  A��\A���A�\)B�B=qB�B��B�B33BQ�B	��B
=BQ�Bp�B�RB�
B�BffB�B��B{B\)B��B�B
=B(�Bp�B�\B�
B ��B"ffB#�B$��B&=qB'�B(��B)�B+33B,Q�B-p�B.�HB0(�B1p�B2�HB4(�B5p�B6�\B7�B8��B:=qB;�B<��B>{B?\)B@��BB{BC33BDz�BE��BF�RBH(�BI��BJ�HBL(�BMp�BNffBO�BP��BR=qBS\)BT��BV=qBW�BX��BY�B[
=B\Q�B]B^�HB`Q�Ba��Bb�HBd(�BeG�Bf�\Bg�
Bi�BjffBk�Bm�BnffBo�Bp��Br=qBs\)Bt��Bu�Bw\)Bx��Bz{B{\)B|��B}�B
=B�=qB��HB���B�Q�B��HB��B�(�B���B��B�=qB��HB��B�(�B��RB�\)B�  B���B�\)B�  B��RB�\)B�  B��\B�33B��
B�z�B�33B��
B��\B�33B�B�Q�B���B��B�Q�B�
=B�B�ffB���B���B�=qB��HB��B�=qB��HB���B�=qB��HB�p�B�{B��RB�G�B�  B���B�\)B�  B��\B�33B��
B�ffB�
=B��B�Q�B�
=B���B�(�B���B�\)B�  B���B�G�B��B�z�B�
=B���B�(�B���B�p�B�{B��RB�G�B��
B�ffB���B���B�(�B���B�p�B�  B��\B�
=B�B�ffB�
=B��B�(�B��RB�p�B�{B���B�p�B�  B�z�B�33B��
B\B�33B��
B�ffB�
=Bř�B�Q�B�
=BǮB�Q�B��HB�p�B�(�B��HB˅B�(�B̸RB�\)B�  BθRB�\)B�{BУ�B�G�B��Bң�B�G�B��Bԏ\B�33B��B֏\B�G�B��B؏\B��B��
Bڏ\B�33B��B�z�B��B�Bޏ\B�G�B�  B��\B�33B��B��B�\)B�  B�\B�G�B�  B�RB�\)B�  B��B�p�B�{B��HB�B�=qB���B홚B�Q�B�
=B�B�ffB��B��
B��B�\)B�  B���B�p�B�(�B���B��B�Q�B�
=B��
B��\B�G�B�  B���B�p�B�=qB���B��C (�C �C �CQ�C�C  CffC��C(�C�C�HC33C��C
=CffC�RC(�C�\C��CQ�C��C{Cz�C�
C	33C	��C
  C
ffC
C(�C�\C��CQ�C�C{C�C�CG�C��C{C�C�HCG�C�RC{Cz�C�
C=qC�C
=CffC�
C=qC�C
=Cp�C�
CG�C��C
=Cz�C�HCG�C��C  CffC��C{CffCC{C\)C��C�HC33Cp�C��C�HC�C\)C�C��C
=C=qCp�C�RC��C�C\)C��C�HC {C G�C �\C ��C!
=C!=qC!p�C!�C!��C"33C"\)C"��C"�
C#{C#Q�C#�C#C$  C$=qC$p�C$��C$�C%(�C%ffC%��C%�HC&�C&Q�C&�C&��C'
=C'=qC'�C'C(  C(33C(p�C(�RC)  C)33C)p�C)��C)��C*(�C*ffC*��C*�HC+�C+Q�C+��C+�
C,
=C,=qC,�C,C-  C-33C-z�C-�RC-��C.(�C.ffC.�C.�C/�C/\)C/��C/�HC0{C0Q�C0�\C0C1
=C1G�C1p�C1�C1��C2(�C2\)C2�\C2�
C3{C3=qC3�C3C3��C4(�C4ffC4�C4�
C5{C5\)C5�\C5�RC6  C6G�C6p�C6�C6��C733C7ffC7��C7�C8{C8Q�C8�\C8C8��C9G�C9z�C9��C9�C:(�C:\)C:��C:�HC;
=C;G�C;�\C;��C<  C<33C<p�C<�C<�HC=(�C=ffC=��C=��C>�C>G�C>�C>��C?
=C?=qC?p�C?�RC?��C@(�C@p�C@��C@�HCA{CA\)CA�\CA��CB{CBG�CBp�CB�RCB��CC(�CCffCC��CC�
CD{CD\)CD�CDCE
=CEG�CEp�CE�RCE��CF�CFffCF�CF�
CG{CG\)CG�CG��CH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                       @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�1A���A�p�A�33A�VA�ĜA���A�l�A�M�A�?}A�(�A�%A��/A���A��jA��FA��A���A���A���A���A���A��PA��7A��A��A��A�|�A�v�A�v�A�p�A�jA�`BA�XA�G�A�C�A�?}A�=qA�9XA�;dA�7LA�/A�$�A��A��A�A��/A��^A���A�x�A�5?A�E�A��A�ȴA�G�A�{A��/A���A�x�A�"�A�x�A���A��9A��A��/A��#A�C�A��A�n�A���A�oA��FA�v�A�/A��+A�ĜA�Q�A�z�A�ffA��\A��yA�33A���A��A�A�z�A��A�33A�hsA���A���A���A� �A�Q�A��FA��A�9XA��!A�33A��hA��A�ffA�VA}�-Az^5Aw|�AuG�As��ArbAp=qAn��Afr�Acx�AaVA_�A^A�A]�hA\��AUAS�^AQ;dAN��ALȴAJbAGp�AC�A?��A;S�A8A�A5?}A3��A2�`A0�HA/;dA.��A-�-A+�A*��A)��A(jA&JA%�A&(�A%�FA#�A!K�A ��A ��A!&�A" �A"  A�7A  A^5A��A��A7LA
=A��A�An�AG�AƨAȴA9XAM�AE�A^5A�DA�AVAZA�7A��A�A��A�HA�7A�uA�yA5?AG�A�/A  Ax�A�RA�wA
��A
5?AȴA�
A�hA�7A%A��A��A��A�!AZA�wAoAĜA�DAE�AƨA�^AbAƨA�`An�A�A v�A 9XA ĜA j@���@�J@�{@��h@���@��;@�|�@�S�@���@�~�@�hs@�bN@�33@�dZ@�  @�Q�@�bN@��@�t�@�\)@�@�(�@��@��;@�t�@�o@���@��y@�b@��@�C�@�p�@�r�@��;@�33@�^@�9@�
=@�n�@�=q@�^@��@�(�@�t�@◍@�p�@��`@߅@�ff@ޗ�@ޟ�@ݺ^@�%@��/@ܬ@�  @۾w@���@��@���@�M�@���@ڇ+@��m@��;@�+@ڗ�@�hs@؛�@�bN@�A�@�l�@�J@�G�@�%@�7L@ԃ@�^5@љ�@щ7@�%@�7L@�?}@�?}@Ь@�j@�(�@Ϯ@�;d@��@�v�@�M�@�@���@�&�@̋D@�9X@�l�@�@�v�@�$�@ɑh@�%@Ȭ@� �@ǥ�@�K�@�33@�{@�hs@���@�Z@�ƨ@�C�@�33@�
=@�$�@���@�`B@��@��@��P@�dZ@�"�@��R@��@���@��`@�z�@���@��m@��w@�S�@���@�ȴ@��!@�V@��@���@�&�@��D@�  @���@�K�@��@���@��@��@���@�p�@��/@��D@�Q�@�1'@��;@�ƨ@��F@��F@��F@��F@��F@���@�"�@�
=@��@��!@�M�@��@���@���@�G�@�?}@�G�@�X@�`B@�X@�p�@��h@��7@�?}@��`@���@�Q�@��;@�\)@���@���@�ff@��@�X@�/@�/@��@�I�@��m@�\)@��@��@��!@�V@�E�@�@��-@�?}@���@�j@�1@���@�;d@�@���@�=q@���@�`B@���@��@��@��m@��m@��;@�C�@��+@�V@�=q@��#@�p�@��@�Ĝ@��u@�A�@��w@�;d@��@���@�V@��^@�X@�?}@��@��@��`@��@� �@���@��@��@��!@��+@�V@�5?@��@�@��@��@��@�p�@���@��u@�bN@�bN@�1'@��;@���@��@��@��H@�~�@�E�@�{@���@�x�@��@���@�Ĝ@��@��u@��D@�ƨ@�33@��@��R@��!@���@��\@��\@�~�@�ff@�=q@��@�@���@�x�@�p�@�O�@�7L@��`@�A�@���@���@��@�l�@�o@���@�ff@�E�@�$�@��#@���@���@��^@��-@��-@��@�G�@���@�9X@��
@��F@���@��@�dZ@�C�@�;d@�33@��@�
=@��H@���@�~�@�5?@���@���@���@��@�x�@�`B@�O�@��@��/@�Ĝ@��@���@��@�Z@�b@���@�|�@�C�@���@���@�^5@��@��@���@���@�O�@�V@��/@���@�bN@�(�@�1@��F@�S�@��!@�V@�-@�J@�J@���@��^@���@��@��@��/@���@��j@���@�z�@�Q�@��@�;@l�@~�y@~ȴ@~$�@}�h@}O�@}V@|��@|�j@|I�@|1@{ƨ@{C�@{@z�\@z�@y��@y��@y%@xQ�@w�@x  @w;d@v�y@vV@u@u��@up�@u/@tI�@st�@r�@r�!@rn�@r�@qx�@pĜ@pQ�@pA�@p �@p  @p  @oK�@nȴ@nV@nE�@n$�@m@l�/@l�j@lZ@k��@k33@k@j��@jJ@i�#@i��@i&�@h �@g�w@g\)@g�@f��@f��@f��@fV@f{@f@e��@eV@d(�@d�@c�
@cƨ@c��@cdZ@c"�@b�H@bn�@b-@a�^@`��@`�@_��@_+@^�R@^{@]��@]�-@]�@]O�@]V@\�j@\z�@\1@[dZ@[@Z��@Z^5@Y��@Y&�@X��@XĜ@XĜ@XĜ@X�@W�@W+@V{@U@U/@TZ@S�F@S"�@R��@R�\@R�@RJ@Q��@Q�#@Q�7@Q7L@Q�@P��@P�`@PĜ@P��@P�u@PA�@Pb@P  @O��@O�@O�P@O\)@Nȴ@N��@N�+@Nv�@Nff@N{@Mp�@M?}@L��@L�/@L��@Lj@L9X@K�@J��@Jn�@I�7@I&�@H�9@HA�@H  @Gl�@G+@F�@Fv�@F{@E��@E�-@EV@D�D@DZ@D�@C�m@CdZ@C@B�H@B=q@A��@A��@AG�@@��@@��@@�@@bN@@b@?�w@?\)@>��@>v�@>5?@>@=�-@=?}@<j@<�@<1@;�m@;�@;S�@:��@:��@:^5@:^5@:=q@9��@9�@9�#@9�^@9�7@9G�@8�`@8Ĝ@7�w@6��@6V@6{@5��@5�-@5�-@5�@5/@4j@41@3ƨ@3�F@3�F@3S�@2�!@2M�@2-@2-@2-@1��@1��@1�@0Ĝ@0�u@0 �@0  @/��@/�@/�P@/�P@/|�@/\)@/�@.�@.��@-��@-�h@-�@-p�@-?}@,�j@,z�@+�m@+33@*�!@*M�@*=q@*-@*�@*�@*�@)��@(�u@'�@'�w@'�@'�P@'K�@'
=@&5?@%�-@%`B@%?}@%/@%�@$��@#�
@#�@#@"�H@"�!@"��@"�\@"n�@"^5@"=q@"�@"J@!��@!��@!�@!�#@!�#@!�#@!�@!�@!�#@!��@!��@!�^@!�^@!��@!��@!x�@!&�@ �9@ bN@ b@�w@\)@
=@�R@��@�+@5?@�-@�h@`B@��@��@�j@�@�D@j@9X@(�@1@�
@ƨ@dZ@��@~�@^5@-@��@�@�@��@�7@G�@�@�`@�9@�@Q�@1'@  @�@�@�@�@�;@�w@�P@l�@+@�@��@��@ff@5?@{@�T@�T@@��@�h@�@`B@O�@/@�@V@�@�/@��@��@�j@�j@�j@�j@�@�D@(�@��@�m@�
@��@�@t�@dZ@S�@C�@33@o@@�\@~�@^5@^5@=q@��@��@��@��@�7@X@&�@%@�A�A��A��A�
=A���A�%A�JA�VA�  A���A��A�z�A�v�A�r�A�VA�C�A�;dA��A�{A�oA�VA���A�ȴA���A��9A���A���A��A�z�A�t�A�ffA�\)A�Q�A�K�A�G�A�A�A�9XA�33A�(�A�$�A�"�A� �A��A��A�{A�bA�%A���A��A��A��`A��;A��;A��A��
A���A���A���A���A���A�ƨA�A��wA��wA��jA��^A��^A��^A��RA��RA��RA��RA��FA��9A��-A��-A��-A��!A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA��\A��\A��PA��PA��PA��PA��PA��PA��DA��7A��+A��A��A��A��A��A��A��A��A��A��A��A�~�A��A��A��A��A��A��A��A��A�~�A�|�A�|�A�z�A�|�A�~�A�~�A�|�A�z�A�x�A�v�A�t�A�t�A�v�A�v�A�v�A�x�A�v�A�t�A�r�A�p�A�n�A�n�A�p�A�p�A�n�A�n�A�l�A�jA�hsA�ffA�hsA�hsA�hsA�dZA�bNA�`BA�^5A�ZA�ZA�XA�XA�ZA�ZA�XA�Q�A�I�A�I�A�G�A�G�A�G�A�G�A�G�A�G�A�C�A�C�A�A�A�A�A�?}A�A�A�A�A�?}A�=qA�;dA�;dA�=qA�?}A�?}A�=qA�;dA�9XA�7LA�9XA�9XA�;dA�;dA�;dA�=qA�=qA�;dA�;dA�9XA�;dA�;dA�9XA�9XA�33A�33A�1'A�/A�-A�-A�-A�/A�/A�-A�(�A�$�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�bA�JA�
=A�A���A���A��A��A��`A��/A��A���A���A���A�ȴA���A��^A��9A��!A���A���A���A���A���A��hA��hA��PA��A�|�A�x�A�t�A�p�A�hsA�^5A�\)A�XA�G�A�7LA���A��/A��#A�=qA�jA�r�A�p�A�XA�9XA�33A�+A� �A��A�  A��wA�jA�9XA��mA��DA�5?A���A��wA�A�A���A�JA�/A���A�5?A���A��A��9A�dZA�5?A�bA���A�hsA��`A�Q�A��TA���A��A�ȴA���A�v�A�-A���A��A�p�A�l�A�ffA�`BA�`BA�^5A�\)A�E�A�5?A��mA�x�A�{A��wA�n�A�$�A�oA���A��`A�ȴA���A��A�ZA�;dA�/A�%A���A�bNA��A��A���A��7A�x�A�n�A�^5A�33A�JA��A���A���A�l�A�=qA�oA��/A���A�r�A�`BA�S�A�G�A�?}A�5?A�+A�{A��/A���A��hA�;dA�1A��#A��A�O�A�A��HA���A��9A���A���A��7A�x�A�bNA�O�A��A��+A��!A�+A�ĜA�x�A�9XA�VA���A���A�n�A�=qA�A�ĜA�t�A�-A��HA���A�S�A��RA��A�VA� �A��wA�ffA�ƨA�dZA�A�ĜA��hA�Q�A�1'A�%A��
A��RA���A�bNA�+A���A���A�S�A�A�A�1'A�$�A��;A��\A�Q�A�+A�VA�%A���A��HA���A��A�v�A�^5A�;dA�VA��TA��-A��A���A���A��uA��+A��A��A�I�A�bA���A�v�A�ZA�?}A�+A�bA���A��yA��/A���A�ĜA��RA��!A���A���A���A���A���A��7A�~�A�r�A�jA�bNA�XA�M�A�G�A�?}A�33A�"�A��A�
=A��;A���A�p�A�dZA�O�A�=qA�/A��A�A��yA��/A��^A���A��PA�t�A�hsA�G�A���A�A�bNA���A��A��+A�n�A�ZA�O�A�E�A�;dA�33A�$�A�{A�A��A���A���A��A�p�A�
=A��
A���A��A�jA�ZA�O�A�G�A�G�A�A�A�7LA�-A�1A��A��;A���A��!A���A�|�A�`BA�A�A�"�A���A��#A��!A�XA�1A���A��7A��+A�~�A�jA�+A�ȴA�S�A�ƨA�`BA� �A���A�;dA���A��^A�r�A�C�A�9XA�5?A�5?A�33A�$�A��A�JA���A��`A���A��RA���A��DA�~�A�l�A�^5A�S�A�K�A�E�A�7LA�/A� �A�{A�1A���A��A��HA���A��^A��DA�=qA���A�l�A� �A���A�5?A��mA���A��!A��7A�ZA��A��A���A��7A�M�A�(�A�JA��A��HA��/A���A�ĜA���A�=qA��!A�  A/A~��A~~�A~=qA~  A}�#A}��A}S�A|�yA|��A|VA{�A{�PAz�`AzJG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                       A��A�1A���A�p�A�33A�VA�ĜA���A�l�A�M�A�?}A�(�A�%A��/A���A��jA��FA��A���A���A���A���A���A��PA��7A��A��A��A�|�A�v�A�v�A�p�A�jA�`BA�XA�G�A�C�A�?}A�=qA�9XA�;dA�7LA�/A�$�A��A��A�A��/A��^A���A�x�A�5?A�E�A��A�ȴA�G�A�{A��/A���A�x�A�"�A�x�A���A��9A��A��/A��#A�C�A��A�n�A���A�oA��FA�v�A�/A��+A�ĜA�Q�A�z�A�ffA��\A��yA�33A���A��A�A�z�A��A�33A�hsA���A���A���A� �A�Q�A��FA��A�9XA��!A�33A��hA��A�ffA�VA}�-Az^5Aw|�AuG�As��ArbAp=qAn��Afr�Acx�AaVA_�A^A�A]�hA\��AUAS�^AQ;dAN��ALȴAJbAGp�AC�A?��A;S�A8A�A5?}A3��A2�`A0�HA/;dA.��A-�-A+�A*��A)��A(jA&JA%�A&(�A%�FA#�A!K�A ��A ��A!&�A" �A"  A�7A  A^5A��A��A7LA
=A��A�An�AG�AƨAȴA9XAM�AE�A^5A�DA�AVAZA�7A��A�A��A�HA�7A�uA�yA5?AG�A�/A  Ax�A�RA�wA
��A
5?AȴA�
A�hA�7A%A��A��A��A�!AZA�wAoAĜA�DAE�AƨA�^AbAƨA�`An�A�A v�A 9XA ĜA j@���@�J@�{@��h@���@��;@�|�@�S�@���@�~�@�hs@�bN@�33@�dZ@�  @�Q�@�bN@��@�t�@�\)@�@�(�@��@��;@�t�@�o@���@��y@�b@��@�C�@�p�@�r�@��;@�33@�^@�9@�
=@�n�@�=q@�^@��@�(�@�t�@◍@�p�@��`@߅@�ff@ޗ�@ޟ�@ݺ^@�%@��/@ܬ@�  @۾w@���@��@���@�M�@���@ڇ+@��m@��;@�+@ڗ�@�hs@؛�@�bN@�A�@�l�@�J@�G�@�%@�7L@ԃ@�^5@љ�@щ7@�%@�7L@�?}@�?}@Ь@�j@�(�@Ϯ@�;d@��@�v�@�M�@�@���@�&�@̋D@�9X@�l�@�@�v�@�$�@ɑh@�%@Ȭ@� �@ǥ�@�K�@�33@�{@�hs@���@�Z@�ƨ@�C�@�33@�
=@�$�@���@�`B@��@��@��P@�dZ@�"�@��R@��@���@��`@�z�@���@��m@��w@�S�@���@�ȴ@��!@�V@��@���@�&�@��D@�  @���@�K�@��@���@��@��@���@�p�@��/@��D@�Q�@�1'@��;@�ƨ@��F@��F@��F@��F@��F@���@�"�@�
=@��@��!@�M�@��@���@���@�G�@�?}@�G�@�X@�`B@�X@�p�@��h@��7@�?}@��`@���@�Q�@��;@�\)@���@���@�ff@��@�X@�/@�/@��@�I�@��m@�\)@��@��@��!@�V@�E�@�@��-@�?}@���@�j@�1@���@�;d@�@���@�=q@���@�`B@���@��@��@��m@��m@��;@�C�@��+@�V@�=q@��#@�p�@��@�Ĝ@��u@�A�@��w@�;d@��@���@�V@��^@�X@�?}@��@��@��`@��@� �@���@��@��@��!@��+@�V@�5?@��@�@��@��@��@�p�@���@��u@�bN@�bN@�1'@��;@���@��@��@��H@�~�@�E�@�{@���@�x�@��@���@�Ĝ@��@��u@��D@�ƨ@�33@��@��R@��!@���@��\@��\@�~�@�ff@�=q@��@�@���@�x�@�p�@�O�@�7L@��`@�A�@���@���@��@�l�@�o@���@�ff@�E�@�$�@��#@���@���@��^@��-@��-@��@�G�@���@�9X@��
@��F@���@��@�dZ@�C�@�;d@�33@��@�
=@��H@���@�~�@�5?@���@���@���@��@�x�@�`B@�O�@��@��/@�Ĝ@��@���@��@�Z@�b@���@�|�@�C�@���@���@�^5@��@��@���@���@�O�@�V@��/@���@�bN@�(�@�1@��F@�S�@��!@�V@�-@�J@�J@���@��^@���@��@��@��/@���@��j@���@�z�@�Q�@��@�;@l�@~�y@~ȴ@~$�@}�h@}O�@}V@|��@|�j@|I�@|1@{ƨ@{C�@{@z�\@z�@y��@y��@y%@xQ�@w�@x  @w;d@v�y@vV@u@u��@up�@u/@tI�@st�@r�@r�!@rn�@r�@qx�@pĜ@pQ�@pA�@p �@p  @p  @oK�@nȴ@nV@nE�@n$�@m@l�/@l�j@lZ@k��@k33@k@j��@jJ@i�#@i��@i&�@h �@g�w@g\)@g�@f��@f��@f��@fV@f{@f@e��@eV@d(�@d�@c�
@cƨ@c��@cdZ@c"�@b�H@bn�@b-@a�^@`��@`�@_��@_+@^�R@^{@]��@]�-@]�@]O�@]V@\�j@\z�@\1@[dZ@[@Z��@Z^5@Y��@Y&�@X��@XĜ@XĜ@XĜ@X�@W�@W+@V{@U@U/@TZ@S�F@S"�@R��@R�\@R�@RJ@Q��@Q�#@Q�7@Q7L@Q�@P��@P�`@PĜ@P��@P�u@PA�@Pb@P  @O��@O�@O�P@O\)@Nȴ@N��@N�+@Nv�@Nff@N{@Mp�@M?}@L��@L�/@L��@Lj@L9X@K�@J��@Jn�@I�7@I&�@H�9@HA�@H  @Gl�@G+@F�@Fv�@F{@E��@E�-@EV@D�D@DZ@D�@C�m@CdZ@C@B�H@B=q@A��@A��@AG�@@��@@��@@�@@bN@@b@?�w@?\)@>��@>v�@>5?@>@=�-@=?}@<j@<�@<1@;�m@;�@;S�@:��@:��@:^5@:^5@:=q@9��@9�@9�#@9�^@9�7@9G�@8�`@8Ĝ@7�w@6��@6V@6{@5��@5�-@5�-@5�@5/@4j@41@3ƨ@3�F@3�F@3S�@2�!@2M�@2-@2-@2-@1��@1��@1�@0Ĝ@0�u@0 �@0  @/��@/�@/�P@/�P@/|�@/\)@/�@.�@.��@-��@-�h@-�@-p�@-?}@,�j@,z�@+�m@+33@*�!@*M�@*=q@*-@*�@*�@*�@)��@(�u@'�@'�w@'�@'�P@'K�@'
=@&5?@%�-@%`B@%?}@%/@%�@$��@#�
@#�@#@"�H@"�!@"��@"�\@"n�@"^5@"=q@"�@"J@!��@!��@!�@!�#@!�#@!�#@!�@!�@!�#@!��@!��@!�^@!�^@!��@!��@!x�@!&�@ �9@ bN@ b@�w@\)@
=@�R@��@�+@5?@�-@�h@`B@��@��@�j@�@�D@j@9X@(�@1@�
@ƨ@dZ@��@~�@^5@-@��@�@�@��@�7@G�@�@�`@�9@�@Q�@1'@  @�@�@�@�@�;@�w@�P@l�@+@�@��@��@ff@5?@{@�T@�T@@��@�h@�@`B@O�@/@�@V@�@�/@��@��@�j@�j@�j@�j@�@�D@(�@��@�m@�
@��@�@t�@dZ@S�@C�@33@o@@�\@~�@^5@^5@=q@��@��@��@��@�7@X@&�@%G�O�A�A��A��A�
=A���A�%A�JA�VA�  A���A��A�z�A�v�A�r�A�VA�C�A�;dA��A�{A�oA�VA���A�ȴA���A��9A���A���A��A�z�A�t�A�ffA�\)A�Q�A�K�A�G�A�A�A�9XA�33A�(�A�$�A�"�A� �A��A��A�{A�bA�%A���A��A��A��`A��;A��;A��A��
A���A���A���A���A���A�ƨA�A��wA��wA��jA��^A��^A��^A��RA��RA��RA��RA��FA��9A��-A��-A��-A��!A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA��\A��\A��PA��PA��PA��PA��PA��PA��DA��7A��+A��A��A��A��A��A��A��A��A��A��A��A�~�A��A��A��A��A��A��A��A��A�~�A�|�A�|�A�z�A�|�A�~�A�~�A�|�A�z�A�x�A�v�A�t�A�t�A�v�A�v�A�v�A�x�A�v�A�t�A�r�A�p�A�n�A�n�A�p�A�p�A�n�A�n�A�l�A�jA�hsA�ffA�hsA�hsA�hsA�dZA�bNA�`BA�^5A�ZA�ZA�XA�XA�ZA�ZA�XA�Q�A�I�A�I�A�G�A�G�A�G�A�G�A�G�A�G�A�C�A�C�A�A�A�A�A�?}A�A�A�A�A�?}A�=qA�;dA�;dA�=qA�?}A�?}A�=qA�;dA�9XA�7LA�9XA�9XA�;dA�;dA�;dA�=qA�=qA�;dA�;dA�9XA�;dA�;dA�9XA�9XA�33A�33A�1'A�/A�-A�-A�-A�/A�/A�-A�(�A�$�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�bA�JA�
=A�A���A���A��A��A��`A��/A��A���A���A���A�ȴA���A��^A��9A��!A���A���A���A���A���A��hA��hA��PA��A�|�A�x�A�t�A�p�A�hsA�^5A�\)A�XA�G�A�7LA���A��/A��#A�=qA�jA�r�A�p�A�XA�9XA�33A�+A� �A��A�  A��wA�jA�9XA��mA��DA�5?A���A��wA�A�A���A�JA�/A���A�5?A���A��A��9A�dZA�5?A�bA���A�hsA��`A�Q�A��TA���A��A�ȴA���A�v�A�-A���A��A�p�A�l�A�ffA�`BA�`BA�^5A�\)A�E�A�5?A��mA�x�A�{A��wA�n�A�$�A�oA���A��`A�ȴA���A��A�ZA�;dA�/A�%A���A�bNA��A��A���A��7A�x�A�n�A�^5A�33A�JA��A���A���A�l�A�=qA�oA��/A���A�r�A�`BA�S�A�G�A�?}A�5?A�+A�{A��/A���A��hA�;dA�1A��#A��A�O�A�A��HA���A��9A���A���A��7A�x�A�bNA�O�A��A��+A��!A�+A�ĜA�x�A�9XA�VA���A���A�n�A�=qA�A�ĜA�t�A�-A��HA���A�S�A��RA��A�VA� �A��wA�ffA�ƨA�dZA�A�ĜA��hA�Q�A�1'A�%A��
A��RA���A�bNA�+A���A���A�S�A�A�A�1'A�$�A��;A��\A�Q�A�+A�VA�%A���A��HA���A��A�v�A�^5A�;dA�VA��TA��-A��A���A���A��uA��+A��A��A�I�A�bA���A�v�A�ZA�?}A�+A�bA���A��yA��/A���A�ĜA��RA��!A���A���A���A���A���A��7A�~�A�r�A�jA�bNA�XA�M�A�G�A�?}A�33A�"�A��A�
=A��;A���A�p�A�dZA�O�A�=qA�/A��A�A��yA��/A��^A���A��PA�t�A�hsA�G�A���A�A�bNA���A��A��+A�n�A�ZA�O�A�E�A�;dA�33A�$�A�{A�A��A���A���A��A�p�A�
=A��
A���A��A�jA�ZA�O�A�G�A�G�A�A�A�7LA�-A�1A��A��;A���A��!A���A�|�A�`BA�A�A�"�A���A��#A��!A�XA�1A���A��7A��+A�~�A�jA�+A�ȴA�S�A�ƨA�`BA� �A���A�;dA���A��^A�r�A�C�A�9XA�5?A�5?A�33A�$�A��A�JA���A��`A���A��RA���A��DA�~�A�l�A�^5A�S�A�K�A�E�A�7LA�/A� �A�{A�1A���A��A��HA���A��^A��DA�=qA���A�l�A� �A���A�5?A��mA���A��!A��7A�ZA��A��A���A��7A�M�A�(�A�JA��A��HA��/A���A�ĜA���A�=qA��!A�  A/A~��A~~�A~=qA~  A}�#A}��A}S�A|�yA|��A|VA{�A{�PAz�`AzJG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                       ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	y�B	v�B	{B	w2B	w2B	u�B	v`B	wfB	u�B	s�B	s�B	r�B	p�B	n�B	o B	o�B	ncB	n�B	o B	o5B	n�B	o5B	o B	oiB	o B	n�B	o5B	n/B	n�B	o B	ncB	n�B	ncB	n/B	m�B	m]B	l�B	m)B	m)B	m)B	m�B	m�B	m�B	m�B	m�B	ncB	o�B	s�B	z�B	��B	��B	҉B
��B
�0B�B-CB0UB0�B<jBMjBZBtTBx�B�7B�=B��B��B�=B��B��B��B��B��B�	B�B�Bw�B`�BT,BgBpBgmBc�BW�BT�BH�BC�B@B4�B($B�B�B
�oB
��B
ӏB
�<B
�B
�B
x8B
n�B
f�B
J�B
4�B
(�B

rB	�8B	��B	��B	�#B	��B	�B	��B	��B	cTB	UgB	J�B	@�B	:^B	5�B	�B	
�B	B��B�B�%B�B�B��B�B�'B��B��B��B�nB��B�nB��B��B�B�XB�HB�qBÖB�yB�pBԕB��B�tB�vBچB�ZB�PB��B�]B��B�gB�gB�B�B		lB	�B	 \B	�B	�B	OB	P�B	CaB	B�B	LdB	OvB	^�B	n/B	v`B	kB	VmB	S�B	O�B	B[B	9$B	,qB	5B	;dB	8�B	7�B	7�B	7B	9XB	49B	-CB	'�B	#:B	IB	�B	�B	#nB	&B	*�B	*0B	.}B	.}B	,qB	+kB	*0B	*0B	-B	/�B	1�B	<�B	@�B	?}B	?�B	8B	7�B	4B	E�B	O�B	FtB	PHB	TaB	WsB	W
B	ZQB	ZB	[#B	[�B	`vB	`�B	`BB	\�B	^jB	g8B	l�B	o5B	tTB	t�B	u�B	z�B	w2B	lWB	c B	d�B	e�B	e�B	jKB	x8B	~�B	~�B	{B	zxB	y�B	~�B	z�B	x�B	v�B	rGB	t�B	y>B	y�B	{�B	~]B	|�B	zB	w�B	{�B	w�B	{JB	�B	�B	�MB	�MB	�B	�SB	��B	�JB	��B	�bB	��B	�VB	�hB	��B	��B	�B	��B	��B	�CB	�IB	�hB	��B	��B	�qB	�6B	�B	�}B	��B	��B	��B	�XB	��B	�}B	�9B	�?B	��B	��B	�XB	��B	��B	�<B	�B	��B	�UB	B	��B	�UB	��B	�[B	��B	��B	�mB	ĜB	ŢB	ŢB	�B	�3B	�aB	�tB	��B	�aB	�9B	��B	�B	��B	�EB	��B	ɆB	�RB	�)B	�0B	�#B	ɆB	ɆB	ȀB	ǮB	��B	�zB	ȀB	��B	��B	��B	�#B	ʌB	��B	��B	�dB	��B	�XB	�#B	�RB	�B	��B	�EB	�B	��B	�B	ĜB	�9B	ŢB	�B	�gB	��B	�B	��B	˒B	̘B	��B	��B	��B	�B	�jB	�B	�TB	ҽB	��B	�[B	�&B	�[B	��B	֡B	�?B	��B	�B	چB	�]B	�B	�TB	�&B	�B	�,B	��B	��B	�B	�8B	�mB	�B	�mB	��B	�8B	��B	�B	�)B	��B	�]B	��B	��B	�B	�B	�B	��B	�ZB	�%B	�+B	��B	�`B	��B	��B	�8B	��B	�B	��B	�B	�B	��B	�B	�PB	��B	��B	��B	��B	�PB	�B	��B	��B	��B	�"B	��B	�VB	�(B	��B	�]B	��B	��B	��B
 4B
 4B
 4B
 iB
 4B
B
oB
B
B
�B
MB
�B
MB
�B
�B
�B
B
SB
SB
�B
�B
YB
_B
�B
�B
fB
	B
	lB

	B

=B

�B
B
PB
�B
B
B
~B
JB
~B
~B
JB
JB
"B
�B
�B
�B
�B
(B
(B
�B
(B
�B
�B
.B
�B
4B
4B
4B
�B
�B
�B
�B
B
�B
�B
�B
SB
$B
�B
YB
�B
+B
+B
�B
_B
+B
�B
�B
_B
�B
�B
	B
	B
=B
=B
�B
�B
�B
�B
B
CB
CB
�B
B
�B
�B
B
�B
OB
�B
�B
OB
�B
VB
�B
�B
�B
!B
 'B
�B
 \B
 �B
 �B
!�B
!�B
"�B
"�B
#B
#B
#�B
#�B
$B
$�B
$�B
%B
%FB
$�B
&B
&LB
(�B
(�B
)_B
)�B
)*B
)�B
)�B
*0B
+�B
+kB
+kB
+kB
+�B
,=B
,=B
,�B
,qB
,=B
-wB
-CB
-CB
-wB
-B
,=B
-B
-�B
-�B
.IB
.}B
.}B
.�B
.�B
/OB
/�B
/B
/�B
/�B
/�B
/�B
0!B
0�B
0�B
1[B
1[B
1[B
2-B
2aB
2-B
2�B
2�B
3�B
3�B
4B
5?B
5B
5tB
6B
6�B
6�B
6zB
7�B
7�B
7�B
7�B
7�B
7�B
8�B
8RB
9�B
:*B
:^B
:*B
:�B
:�B
:�B
:�B
:�B
;dB
;0B
:�B
;0B
;�B
;dB
;�B
<B
<B
;�B
<6B
=qB
=�B
=�B
=�B
=�B
>B
>BB
>wB
>�B
?HB
?B
?�B
?�B
@OB
@�B
AUB
A�B
A�B
A�B
B'B
B[B
B�B
B�B
CaB
C-B
C�B
C�B
D�B
D�B
EB
FB
F?B
FtB
FtB
FtB
F?B
FtB
GEB
HKB
HB
G�B
H�B
IRB
I�B
J�B
J�B
K^B
K^B
K�B
K^B
K�B
L0B
L�B
L�B
L�B
L�B
M6B
M6B
MjB
N<B
N<B
NpB
N�B
N�B
N�B
OvB
QB
P�B
QB
QB
P�B
R B
R�B
R�B
R�B
R�B
S&B
R�B
S&B
TaB
TaB
U2B
U�B
U�B
V9B
V�B
V�B
WsB
WsB
W�B
XEB
XyB
XyB
X�B
YB
YB
Y�B
Y�B
Y�B
ZQB
ZQB
ZQB
Z�B
Z�B
Z�B
[#B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
\]B
\)B
\)B
\]B
\)B
\]B
\]B
\�B
\�B
\]B
\�B
\�B
\�B
]dB
]dB
^B
^B
^5B
^jB
^�B
^�B
^�B
^�B
_;B
_;B
_B
`B
`�B
`BB
`�B
`�B
aHB
a�B
bB
bNB
b�B
bB
b�B
bNB
b�B
b�B
c�B
c B
b�B
b�B
b�B
c B
c�B
dZB
dZB
d�B
e,B
e`B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
f2B
f�B
g8B
gmB
gmB
gmB
g�B
h>B
h>B
iDB
iyB
jB
jB
jKB
jKB
jB
jB
i�B
jKB
k�B
k�B
k�B
k�B
l"B
lWB
lWB
m)B
l�B
m]B
m)B
m)B
m)B
o B
o�B
oiB
pB
pB
pB
pB
p;B
p;B
p;B
poB
poB
p�B
p�B
p�B
qAB
qAB
qB
qAB
qAB
qAB
qvB
qvB
qAB
qAB
qvB
qvB
qvB
qvB
rB
rGB
rGB
r|B
r�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
v`B
v�B
v�B
v�B
v�B
v�B
w2B
w2B
w2B
wfB
w2B
w2B
x8B
xlB
xlB
x�B
x�B
y	B
y>B
y>B
zB
zDB
{JB
{B
{JB
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|�B
|PB
|�B
|�B
}"B
}"B
}VB
}�B
}�B
}�B
~(B
~(B
~]B
~]B
~�B
~�B
~�B
~�B
.B
.B
.B
cB
�B
�B
�B
�B
�B
�B
�B
�B
� B
�iB
��B
�iB
�iB
�B
�B
�B
�;B
�;B
�;B
�;B
�oB
�oB
�AB
�AB
�B
�B
�AB
��B
��B
��B
��B
�B
�B
�{B
��B
�B	}�B	w�B	xB	rB	y>B	z�B	t�B	v�B	s�B	xlB	x8B	u�B	xB	v+B	}VB	w2B	t�B	{�B	u�B	t�B	t�B	|B	wfB	x�B	u%B	v�B	xB	x�B	u�B	uZB	t�B	uZB	tB	s�B	sMB	sMB	sB	sB	s�B	sB	r�B	r|B	q�B	rB	qB	qAB	qvB	p�B	qAB	m�B	n�B	ncB	m�B	n�B	n�B	ncB	n/B	o B	oiB	n�B	oiB	o�B	o�B	o�B	o�B	o�B	oiB	oiB	oiB	o B	n�B	n�B	m�B	m�B	m�B	n/B	n/B	n/B	ncB	n�B	o5B	o B	o�B	o�B	oiB	oiB	m�B	m�B	ncB	ncB	o B	oiB	oiB	oiB	oiB	o5B	ncB	ncB	n�B	n�B	n�B	n�B	oiB	o B	oiB	o B	o�B	pB	o�B	o5B	n�B	m�B	n/B	o�B	pB	o�B	pB	o�B	o5B	n�B	ncB	ncB	ncB	n�B	oiB	oiB	oiB	o5B	o B	ncB	ncB	ncB	n/B	ncB	o5B	oiB	o5B	o5B	o5B	n�B	ncB	m�B	m�B	m�B	m�B	n�B	oiB	o5B	o5B	ncB	n/B	m�B	ncB	n�B	n�B	n�B	oiB	oiB	o5B	n�B	n/B	m�B	m�B	n�B	oiB	o�B	o5B	o5B	n/B	m�B	m�B	m�B	m�B	ncB	o B	o5B	n�B	m�B	m�B	m�B	m�B	ncB	ncB	o B	ncB	n�B	m�B	m]B	m)B	m�B	m�B	m�B	m�B	m�B	j�B	m]B	lWB	k�B	k�B	l�B	l�B	m]B	m)B	m�B	l�B	l�B	l�B	l�B	m]B	m�B	m)B	m]B	l�B	l�B	m]B	m]B	m�B	m)B	m]B	l�B	l�B	lWB	lWB	l�B	n/B	n�B	o B	o�B	n�B	m�B	m]B	o B	m�B	m�B	n/B	ncB	n/B	m]B	m]B	l�B	m)B	n�B	n/B	o B	m�B	m�B	m�B	l�B	l�B	l�B	m�B	o5B	ncB	ncB	n�B	n�B	n/B	m�B	n/B	n�B	o5B	pB	pB	p;B	qB	qB	rGB	tB	s�B	u�B	v+B	u�B	v�B	x�B	z�B	|�B	~�B	�oB	�{B	�B	��B	�%B	�_B	��B	�lB	��B	��B	�MB	��B	��B	�jB	�aB	�B	�6B	�dB	��B	�B	��B
7�B
}VB
�7B
��B
�FB
��B
��B
B
�zB
��B
�<B
�2B
�
B
�	BB$B"�B �B�BxB($B,qB<6B/B+�B*�B%B>wB@�B*0B*eB'B-wB.}B8RB>wB-B1[BC�B8�B5�B=B@�B?�BNBO�BLdBM�BM�BN�BO�BRTBYBX�Bh�BgmBq�Bs�Bx�Bv`Bu�Bv`Bo�BxBx8B{�B� B|PB|�B�;B��B�B�~B�xB��B��B��B��B�qB��B�!B�B�!B��B��B�XB��B�kB��B�B�qB��B��B�B�0B��B�IB��B�B��B��B��B�<B�B�?B�EB�HB�OB�B�6B��B�^B�^B�B�B��B��B��B��B��B�tB�OB��B�'B��B�qB��B��B�CB��B�_B��B��B�bB��B.B{JB��B~�B|�B.Bs�Bn�Bg�Be�Bc�BZQB[#BT�BOvBNpBOBBUgBjKBiBh�BaBdZBf2Bu�Bq�Br�Bm]Bi�Be�Bh�Bh�Bd�BhsBf�B_�Bj�Bc�BaHBaBW�BYBY�BW
BYBT�BS�Bd�B[�B_BQNBQ�BQ�BOBBO�BNpBK�BJ#BIBI�BH�BF�BE�BFBE�BEmBE9BD3BC�BD�BB�BA�BA�BA�BA B@B?HB>�B=�B?HBB'BA B:�B8B5tB4B33B5�B.�B-�B-�B)�B)�B'�B'�B#�B$tB,�B!�B(�B%FB�BB�B�B�BxB
rB
	B	BfB�BYB�B�B
�B�B
��B
��B
�B
��B
�pB
�ZB
�NB
��B
�B
��B
�B
��B
�|B
�)B
�B
��B
�KB
רB
��B
�,B
�[B
�TB
͟B
�dB
��B
ϫB
�EB
��B
�B
��B
�aB
�hB
��B
�^B
��B
��B
�XB
��B
��B
�$B
��B
�hB
��B
�B
~�B
}�B
|�B
}"B
~]B
{�B
z�B
{�B
{B
zxB
zxB
wfB
v�B
s�B
u�B
tB
qAB
p;B
pB
qAB
m�B
l"B
ncB
kB
iB
iB
ffB
e`B
e�B
e�B
e�B
jKB
Z�B
XyB
Z�B
RTB
H�B
@OB
AUB
>�B
?HB
>wB
:�B
8�B
9�B
6�B
1�B
/OB
-CB
,B
(�B
'RB
'�B
'�B
.B
1�B
/OB
�B
FB
�B
�B
�B
_B
	7B
�B
	B
�B	�cB	�.B	��B	��B
{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2021040401244920210404012449IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021041400004320210414000043QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021041400004320210414000043QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014520210427140145IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20220504162938              CF      PSAL                            ?�  G�O�D�7
G�O�?�  G�O�Sensor Failure                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20220504163448    CF                  PSAL            G�O�?\)G�O�CH
=G�O�?�                  Sensor Failure  SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                
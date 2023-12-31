CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-05-13T10:23:25Z creation; 2023-04-26T19:14:26Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Ҩ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � hH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180513102325  20230426191426  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_012                 7316_008644_012                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�bY?���@�bY?���11  @�bY���@�bY���@*=�����@*=������d3�B��9�d3�B��911  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?���@   @@  @�  @�  @�  @޸R@��RA\)A\)A+�A@��A`��A\)A��A�  A�Q�A�Q�AУ�A�Q�A�A��B  B  B�
B   B(  B0(�B8(�B?�
BG�
BP  BX(�B`(�Bh  Bp  Bx(�B�  B�{B�=qB���B�B�B��B�  B�  B�  B�  B�  B�  B�  B��B�  B�{B�{B��B��B�  B�  B��B�  B�  B��B�  B��B��B��B��
B��C   C
=C
=C{C  C	��C��C  C
=C  C�C��C��C��C
=C  C   C"
=C$  C%��C'�C)��C,
=C.  C/��C1��C4
=C6  C7��C:  C<  C>  C@
=CB
=CD  CF
=CH
=CJ{CL  CN  CP
=CR
=CT
=CV  CX  CY��C\  C^
=C`  Cb  Cd  Cf  Ch  Cj
=Cl  Cm��Cp  Cq��Cs�Cu��Cx
=Cz
=C|  C}��C�C���C�  C�  C�C�  C�C�C���C�  C�  C���C�  C�
=C�C���C���C�  C���C���C�  C�  C�
=C�  C�  C�  C�  C�  C���C�  C�C�  C���C�  C�C���C�  C�  C�  C�  C�  C�  C�  C�C���C�  C�C�C�  C���C���C�  C�C�C�  C���C���C���C�  C�C���C�  C�  C���C�  C�C�  C�C�  C���C�  C�  C�  C�  C���C���C�C�C�  C�  C�  C�C�C�  C���C���C���C�  C�
=C�  C���C���C���C���C���C���C���C�  C�  C���C�C���C���C�C�  C�  C�  C�C�C�C�  C�  C���C���C���C���C�C�C�C�C�  C���C�  C�  C���C�  C���C���D   D }qD  D��D  D� D  D� D  D��D  D� D�qD}qD�D��D  Dz�D�qD	� D
  D
� D
�qD��DD�D  D� DD� D��D� D�D� D  D}qD�qD� D  D��D�D}qD�qD� D�D��D  D}qD  D��D�D� D�qD� D  D� D�D�D�D� D  D� D�qD}qD�qD ��D!�D!z�D!��D"}qD"�qD#}qD#�qD$}qD%  D%�D&D&��D'�D'� D'�qD(��D)D)�D*�D*�D+D+� D,�D,��D,�qD-}qD.  D.� D/  D/}qD/�qD0}qD0�qD1}qD2  D2}qD2�qD3}qD4  D4��D5D5� D5��D6� D7�D7��D8  D8z�D8��D9}qD:  D:}qD;  D;��D;�qD<}qD<�qD=� D>  D>}qD?�D?��D@�D@� DA  DA}qDB  DB}qDB�qDC��DD�DD}qDD�qDE��DF  DF}qDF��DG}qDH�DH�DI�DI� DJ�DJ� DJ�qDK� DL  DL}qDL�qDM� DN�DN}qDO�DO�DP�DP� DQ  DQ}qDR  DR}qDR��DS� DS�qDTz�DT�qDU��DV  DV� DW  DW}qDX  DX��DY  DY� DZ  DZ� D[  D[}qD\  D\� D\�qD]}qD]�qD^}qD^��D_}qD_�qD`}qD`�qDaz�Da��Dbz�Db�qDc��Dd�Dd��De�De}qDe�qDf� Dg  Dg��Dh�Dh� Di�Di� Dj  Dj��Dk  Dk}qDk�qDl� Dl�qDm� Dn�Dn� Dn�qDo� Dp�Dp� Dq  Dq� Dr�Dr}qDs  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv�qDw}qDw��Dx� Dy  Dy� Dz�Dz��D{�D{� D|  D|� D}  D}}qD}�qD~}qD~�qD}qD�  D�AHD�� D���D���D�>�D�~�D��HD�  D�@ D�� D���D�  D�@ D�~�D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D���D���D�@ D�� D�� D�HD�@ D�~�D�� D�  D�@ D��HD��HD�HD�@ D�}qD���D���D�>�D�~�D�� D�  D�>�D��HD�� D�  D�>�D�~�D���D�  D�AHD�}qD��qD��qD�=qD�� D��HD���D�@ D�}qD��qD���D�@ D��HD��HD�  D�@ D�� D��HD�HD�@ D�� D�� D�  D�@ D�~�D���D�HD�@ D�� D���D��qD�>�D�~�D�� D�  D�AHD��HD���D���D�=qD�~�D���D�  D�AHD�� D���D��qD�=qD�~�D�� D���D�=qD�~�D�� D���D�@ D�� D��qD�  D�@ D�~�D���D�  D�AHD��HD�� D�  D�AHD�~�D�� D�HD�>�D�}qD�� D�HD�B�D��HD��HD�  D�>�D��HD�� D�  D�AHD��HD�� D���D�>�D�� D���D��qD�>�D��HD�� D��qD�>�D�� D��HD�  D�>�D��HD�� D��qD�=qD��HD�� D�  D�>�D�}qD�� D��D�AHD�~�D��qD��qD�>�D��HD�D���D�>�D�~�D���D�  D�AHD��HD�� D���D�=qD�~�D���D�  D�@ D�� D���D�  D�AHD�~�D��qD���D�AHD���D��HD���D�@ D�� D�� D�HD�>�D�� D�� D���D�@ D��HD�� D���D�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D��HD�D��D�@ D�~�D��HD�HD�@ D�� D��HD�  D�@ D�~�D��qD���D�>�D�~�D�� D�  D�@ D��HD�� D���D�>�D�~�D���D���D�>�D�� D��HD�  D�>�D�� D��HD�  D�@ D�� D���D���D�>�D�~�D���D�  D�AHD D¾�D���D�>�D�~�D�� D�  D�>�D�~�Dľ�D�  D�@ D�~�Dž�D�HD�B�DƁHD�� D�  D�@ Dǀ D�� D�  D�AHDȁHD�� D�  D�AHDɀ Dɾ�D���D�@ D�~�Dʾ�D�  D�@ Dˀ D��HD�  D�@ D́HD�D�HD�@ D́HD��HD�HD�B�D΀ D�� D�HD�AHD�~�D�� D�  D�>�DЀ D�� D�HD�@ D�~�DѾ�D�  D�AHD҂�D��HD�  D�AHDӁHD��HD�  D�AHDԀ D�� D�  D�B�DՂ�D��HD�  D�>�D�~�D�� D�HD�@ DׁHD�� D�  D�AHD؁HD��HD�HD�@ DفHDپ�D�  D�@ Dڀ D��HD��D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�AHD݁HD��HD�  D�AHDނ�D��HD�  D�@ D߁HD�D�HD�@ D�� DྸD���D�@ D�HD�D�  D�=qD�}qD⾸D�HD�@ D�}qD㾸D���D�@ D�~�D侸D�  D�@ D� D�� D�  D�@ D�~�D澸D��D�AHD� D羸D�  D�AHD� D�� D�  D�AHD�HD�� D���D�@ D�HD�� D���D�=qD�~�D�� D�  D�B�D�HD��HD�  D�>�D�~�D��qD���D�>�D�}qD��HD��D�B�D�HD��HD��D�AHD��HD�qD�  D�AHD�HD��HD�HD�B�D� D�qD��qD�>�D� D�D�  D�>�D�~�D���D�HD�B�D�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D�HD�AHD�� D�� D�  D�@ D�~�D��HD��D�(�D�e?k�?u?���?�Q�?�G�?��H@
=q@��@(��@:�H@L��@Y��@k�@��\@���@��@���@��\@�{@�
=@��R@Ǯ@��@��H@��
@�@�z�@��RA�
A��Ap�AG�AA�HA   A#�
A(Q�A,��A1�A6ffA:�HA>�RAC�
AI��AN{AQ�AVffA[�A`��AeAi��An{As�
AxQ�A|(�A�  A��\A��A�\)A�G�A��A�A���A��HA��A�
=A�G�A��A�{A�Q�A�=qA�z�A�
=A�G�A��HA���A��A��A��A�p�A��A�=qA�(�A�ffA�  A\A���A�
=A���A��HA��AϮA��A��
A�p�A�  A�=qA���A�
=A��A��HA��A�A陚A�A�p�A�A�=qA�(�A�ffA�  A�=qA���A�
=B z�Bp�BffB�B��BB�\B�B��B	�B
�HB�B��B{B33B(�B��B�B
=B(�BG�B{B
=B  B�B=qB33B  B�B=qB\)B z�B!G�B"=qB#\)B$z�B%��B&ffB'�B(z�B)B*�RB+�B,��B-�B.�RB/�B0��B2{B3
=B4  B4��B5�B733B8Q�B9G�B:=qB;33B<(�B=G�B>ffB?�B@z�BAp�BB=qBC\)BDz�BE��BF�RBG�BHz�BIp�BJ�\BK�BL��BMBN�\BO�BP��BQ�BS
=BS�
BT��BUBV�HBX  BY�BZ{B[
=B\  B\��B^=qB_33B`Q�BaG�Bb{Bc33BdQ�Bep�Bf�\Bg�Bhz�Bip�BjffBk�Bl��Bm��Bn�RBo�Bpz�Bqp�BrffBs�Btz�Bup�BvffBw\)BxQ�ByG�BzffB{�B|z�B}p�B~=qB33B�{B���B�33B��B�(�B��RB�33B�B�ffB��HB�\)B��
B�Q�B���B�G�B�B�Q�B��RB�
=B�G�B���B��
B�{B�=qB�Q�B�ffB�ffB�ffB�ffB�ffB�z�B��\B��\B��\B�z�B�z�B��\B��\B���B��RB��RB��RB���B���B���B���B��HB���B�
=B�
=B�
=B�
=B��B�33B�G�B�\)B�p�B�p�B�p�B�p�B��B���B��B�B��B��B�{B�(�B�(�B�=qB�=qB�Q�B�ffB�z�B��\B��RB��RB���B���B��B�33B�33B�33B�G�B�p�B��B��B�B��
B�  B�{B�(�B�(�B�=qB�Q�B�z�B���B��RB��HB�
=B�
=B��B�33B�\)B�p�B���B�B��B�  B�{B�(�B�Q�B�z�B���B���B���B��B�\)B�p�B���B�B��B�{B�Q�B��\B���B�
=B�\)B��B�B�{B�Q�B��\B���B��B�p�B�B�{B�z�B���B��B��B��B�=qB���B�
=B�p�B��
B�=qB���B�
=B�p�B��
B�=qB��\B���B�p�B�B�=qB��\B���B�G�B�B�{B�z�B��HB�G�B��B�{B�z�B��HB�G�B��B�{B�z�B��HB�G�B��B�{B�z�B���B�\)B�B�(�B��\B���B�\)B��
B�=qB���B�
=B�p�B��
B�=qB��RB��B���B�  B�z�B��HB�\)B�B�Q�B��RB�33B��B�{B���B�
=B��B��B�ffB���B�G�B��B�(�B��\B�
=B��B��B�z�B��HB�\)B�B�Q�B��RB�33B�B�(�B��RB�33B�B�=qB���B�33B���B�{B��\B�
=B�p�B��B�ffB��HB�\)B��B�z�B���B�p�B�  B�z�B��HB�\)B��
B�Q�B��HB�p�B�  Bʏ\B��B˙�B�{B̏\B�
=B͙�B�(�BΣ�B�33B��
B�Q�B��HB�p�B��B�ffB���BӅB�{Bԣ�B�G�B��B�ffB���BׅB�  B؏\B�33B��
B�ffB��HB�p�B�  B܏\B��B�B�ffB���B߅B�{B�RB�p�B�{B�RB�\)B��B�\B��B��
B�z�B�G�B��
B�z�B��B�B�ffB�
=B�B�z�B�33B��
B�z�B�
=B�B�z�B�33B��
B�z�B�33B��
B�\B�G�B�  B���B�\)B�  B���B�p�B�(�B���B���B�=qB��HB���B�Q�B��B��
C G�C ��C ��CG�C��C
=CffCC{Cp�CC�Cz�C�
C33C�C�
C33C��C��CQ�C�C  C\)C�RC	�C	z�C	��C
(�C
z�C
�
C33C�\C�HC33C�C�HCG�C�\C�C=qC��C��CQ�C��C�CG�C��C  C\)C��C��CG�C�C
=CQ�C��C��CG�C��C  CQ�C��C�CG�C��C�C=qC�C�HC=qC�C��C�Cp�CC
=CQ�C��C  CQ�C��C�
C(�C�C��C{C\)C�RC
=CQ�C�\C�HC=qC�C��C {C \)C �RC!  C!G�C!�C!��C"(�C"p�C"�C"�C#33C#�\C#��C$
=C$Q�C$��C$�C%33C%p�C%�C&  C&Q�C&��C&�HC'�C'ffC'C(
=C(G�C(�C(��C)�C)p�C)�RC)��C*=qC*�\C*�
C+�C+\)C+�C,  C,G�C,�C,��C-(�C-p�C-�C.  C.Q�C.��C.�C/(�C/z�C/�
C0{C0\)C0�C1  C1Q�C1�\C1�HC233C2�\C2�
C3{C3ffC3C4{C4\)C4��C4��C5Q�C5��C5�C633C6�\C6�HC7(�C7z�C7��C8(�C8p�C8�RC9{C9ffC9�RC:  C:Q�C:�C:��C;G�C;��C;��C<=qC<�C<�HC==qC=z�C=�
C>33C>z�C>C?�C?z�C?�RC@
=C@ffC@�RCA  CAG�CA��CA��CBG�CB�\CB�CC=qCC�CC�
CD33CDz�CDCE{CEp�CECF
=CFQ�CF�CG  CG=qCG�\CG�
CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                   ?���@   @@  @�  @�  @�  @޸R@��RA\)A\)A+�A@��A`��A\)A��A�  A�Q�A�Q�AУ�A�Q�A�A��B  B  B�
B   B(  B0(�B8(�B?�
BG�
BP  BX(�B`(�Bh  Bp  Bx(�B�  B�{B�=qB���B�B�B��B�  B�  B�  B�  B�  B�  B�  B��B�  B�{B�{B��B��B�  B�  B��B�  B�  B��B�  B��B��B��B��
B��C   C
=C
=C{C  C	��C��C  C
=C  C�C��C��C��C
=C  C   C"
=C$  C%��C'�C)��C,
=C.  C/��C1��C4
=C6  C7��C:  C<  C>  C@
=CB
=CD  CF
=CH
=CJ{CL  CN  CP
=CR
=CT
=CV  CX  CY��C\  C^
=C`  Cb  Cd  Cf  Ch  Cj
=Cl  Cm��Cp  Cq��Cs�Cu��Cx
=Cz
=C|  C}��C�C���C�  C�  C�C�  C�C�C���C�  C�  C���C�  C�
=C�C���C���C�  C���C���C�  C�  C�
=C�  C�  C�  C�  C�  C���C�  C�C�  C���C�  C�C���C�  C�  C�  C�  C�  C�  C�  C�C���C�  C�C�C�  C���C���C�  C�C�C�  C���C���C���C�  C�C���C�  C�  C���C�  C�C�  C�C�  C���C�  C�  C�  C�  C���C���C�C�C�  C�  C�  C�C�C�  C���C���C���C�  C�
=C�  C���C���C���C���C���C���C���C�  C�  C���C�C���C���C�C�  C�  C�  C�C�C�C�  C�  C���C���C���C���C�C�C�C�C�  C���C�  C�  C���C�  C���C���D   D }qD  D��D  D� D  D� D  D��D  D� D�qD}qD�D��D  Dz�D�qD	� D
  D
� D
�qD��DD�D  D� DD� D��D� D�D� D  D}qD�qD� D  D��D�D}qD�qD� D�D��D  D}qD  D��D�D� D�qD� D  D� D�D�D�D� D  D� D�qD}qD�qD ��D!�D!z�D!��D"}qD"�qD#}qD#�qD$}qD%  D%�D&D&��D'�D'� D'�qD(��D)D)�D*�D*�D+D+� D,�D,��D,�qD-}qD.  D.� D/  D/}qD/�qD0}qD0�qD1}qD2  D2}qD2�qD3}qD4  D4��D5D5� D5��D6� D7�D7��D8  D8z�D8��D9}qD:  D:}qD;  D;��D;�qD<}qD<�qD=� D>  D>}qD?�D?��D@�D@� DA  DA}qDB  DB}qDB�qDC��DD�DD}qDD�qDE��DF  DF}qDF��DG}qDH�DH�DI�DI� DJ�DJ� DJ�qDK� DL  DL}qDL�qDM� DN�DN}qDO�DO�DP�DP� DQ  DQ}qDR  DR}qDR��DS� DS�qDTz�DT�qDU��DV  DV� DW  DW}qDX  DX��DY  DY� DZ  DZ� D[  D[}qD\  D\� D\�qD]}qD]�qD^}qD^��D_}qD_�qD`}qD`�qDaz�Da��Dbz�Db�qDc��Dd�Dd��De�De}qDe�qDf� Dg  Dg��Dh�Dh� Di�Di� Dj  Dj��Dk  Dk}qDk�qDl� Dl�qDm� Dn�Dn� Dn�qDo� Dp�Dp� Dq  Dq� Dr�Dr}qDs  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv�qDw}qDw��Dx� Dy  Dy� Dz�Dz��D{�D{� D|  D|� D}  D}}qD}�qD~}qD~�qD}qD�  D�AHD�� D���D���D�>�D�~�D��HD�  D�@ D�� D���D�  D�@ D�~�D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D���D���D�@ D�� D�� D�HD�@ D�~�D�� D�  D�@ D��HD��HD�HD�@ D�}qD���D���D�>�D�~�D�� D�  D�>�D��HD�� D�  D�>�D�~�D���D�  D�AHD�}qD��qD��qD�=qD�� D��HD���D�@ D�}qD��qD���D�@ D��HD��HD�  D�@ D�� D��HD�HD�@ D�� D�� D�  D�@ D�~�D���D�HD�@ D�� D���D��qD�>�D�~�D�� D�  D�AHD��HD���D���D�=qD�~�D���D�  D�AHD�� D���D��qD�=qD�~�D�� D���D�=qD�~�D�� D���D�@ D�� D��qD�  D�@ D�~�D���D�  D�AHD��HD�� D�  D�AHD�~�D�� D�HD�>�D�}qD�� D�HD�B�D��HD��HD�  D�>�D��HD�� D�  D�AHD��HD�� D���D�>�D�� D���D��qD�>�D��HD�� D��qD�>�D�� D��HD�  D�>�D��HD�� D��qD�=qD��HD�� D�  D�>�D�}qD�� D��D�AHD�~�D��qD��qD�>�D��HD�D���D�>�D�~�D���D�  D�AHD��HD�� D���D�=qD�~�D���D�  D�@ D�� D���D�  D�AHD�~�D��qD���D�AHD���D��HD���D�@ D�� D�� D�HD�>�D�� D�� D���D�@ D��HD�� D���D�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D��HD�D��D�@ D�~�D��HD�HD�@ D�� D��HD�  D�@ D�~�D��qD���D�>�D�~�D�� D�  D�@ D��HD�� D���D�>�D�~�D���D���D�>�D�� D��HD�  D�>�D�� D��HD�  D�@ D�� D���D���D�>�D�~�D���D�  D�AHD D¾�D���D�>�D�~�D�� D�  D�>�D�~�Dľ�D�  D�@ D�~�Dž�D�HD�B�DƁHD�� D�  D�@ Dǀ D�� D�  D�AHDȁHD�� D�  D�AHDɀ Dɾ�D���D�@ D�~�Dʾ�D�  D�@ Dˀ D��HD�  D�@ D́HD�D�HD�@ D́HD��HD�HD�B�D΀ D�� D�HD�AHD�~�D�� D�  D�>�DЀ D�� D�HD�@ D�~�DѾ�D�  D�AHD҂�D��HD�  D�AHDӁHD��HD�  D�AHDԀ D�� D�  D�B�DՂ�D��HD�  D�>�D�~�D�� D�HD�@ DׁHD�� D�  D�AHD؁HD��HD�HD�@ DفHDپ�D�  D�@ Dڀ D��HD��D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�AHD݁HD��HD�  D�AHDނ�D��HD�  D�@ D߁HD�D�HD�@ D�� DྸD���D�@ D�HD�D�  D�=qD�}qD⾸D�HD�@ D�}qD㾸D���D�@ D�~�D侸D�  D�@ D� D�� D�  D�@ D�~�D澸D��D�AHD� D羸D�  D�AHD� D�� D�  D�AHD�HD�� D���D�@ D�HD�� D���D�=qD�~�D�� D�  D�B�D�HD��HD�  D�>�D�~�D��qD���D�>�D�}qD��HD��D�B�D�HD��HD��D�AHD��HD�qD�  D�AHD�HD��HD�HD�B�D� D�qD��qD�>�D� D�D�  D�>�D�~�D���D�HD�B�D�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D�HD�AHD�� D�� D�  D�@ D�~�D��HD��D�(�G�O�?k�?u?���?�Q�?�G�?��H@
=q@��@(��@:�H@L��@Y��@k�@��\@���@��@���@��\@�{@�
=@��R@Ǯ@��@��H@��
@�@�z�@��RA�
A��Ap�AG�AA�HA   A#�
A(Q�A,��A1�A6ffA:�HA>�RAC�
AI��AN{AQ�AVffA[�A`��AeAi��An{As�
AxQ�A|(�A�  A��\A��A�\)A�G�A��A�A���A��HA��A�
=A�G�A��A�{A�Q�A�=qA�z�A�
=A�G�A��HA���A��A��A��A�p�A��A�=qA�(�A�ffA�  A\A���A�
=A���A��HA��AϮA��A��
A�p�A�  A�=qA���A�
=A��A��HA��A�A陚A�A�p�A�A�=qA�(�A�ffA�  A�=qA���A�
=B z�Bp�BffB�B��BB�\B�B��B	�B
�HB�B��B{B33B(�B��B�B
=B(�BG�B{B
=B  B�B=qB33B  B�B=qB\)B z�B!G�B"=qB#\)B$z�B%��B&ffB'�B(z�B)B*�RB+�B,��B-�B.�RB/�B0��B2{B3
=B4  B4��B5�B733B8Q�B9G�B:=qB;33B<(�B=G�B>ffB?�B@z�BAp�BB=qBC\)BDz�BE��BF�RBG�BHz�BIp�BJ�\BK�BL��BMBN�\BO�BP��BQ�BS
=BS�
BT��BUBV�HBX  BY�BZ{B[
=B\  B\��B^=qB_33B`Q�BaG�Bb{Bc33BdQ�Bep�Bf�\Bg�Bhz�Bip�BjffBk�Bl��Bm��Bn�RBo�Bpz�Bqp�BrffBs�Btz�Bup�BvffBw\)BxQ�ByG�BzffB{�B|z�B}p�B~=qB33B�{B���B�33B��B�(�B��RB�33B�B�ffB��HB�\)B��
B�Q�B���B�G�B�B�Q�B��RB�
=B�G�B���B��
B�{B�=qB�Q�B�ffB�ffB�ffB�ffB�ffB�z�B��\B��\B��\B�z�B�z�B��\B��\B���B��RB��RB��RB���B���B���B���B��HB���B�
=B�
=B�
=B�
=B��B�33B�G�B�\)B�p�B�p�B�p�B�p�B��B���B��B�B��B��B�{B�(�B�(�B�=qB�=qB�Q�B�ffB�z�B��\B��RB��RB���B���B��B�33B�33B�33B�G�B�p�B��B��B�B��
B�  B�{B�(�B�(�B�=qB�Q�B�z�B���B��RB��HB�
=B�
=B��B�33B�\)B�p�B���B�B��B�  B�{B�(�B�Q�B�z�B���B���B���B��B�\)B�p�B���B�B��B�{B�Q�B��\B���B�
=B�\)B��B�B�{B�Q�B��\B���B��B�p�B�B�{B�z�B���B��B��B��B�=qB���B�
=B�p�B��
B�=qB���B�
=B�p�B��
B�=qB��\B���B�p�B�B�=qB��\B���B�G�B�B�{B�z�B��HB�G�B��B�{B�z�B��HB�G�B��B�{B�z�B��HB�G�B��B�{B�z�B���B�\)B�B�(�B��\B���B�\)B��
B�=qB���B�
=B�p�B��
B�=qB��RB��B���B�  B�z�B��HB�\)B�B�Q�B��RB�33B��B�{B���B�
=B��B��B�ffB���B�G�B��B�(�B��\B�
=B��B��B�z�B��HB�\)B�B�Q�B��RB�33B�B�(�B��RB�33B�B�=qB���B�33B���B�{B��\B�
=B�p�B��B�ffB��HB�\)B��B�z�B���B�p�B�  B�z�B��HB�\)B��
B�Q�B��HB�p�B�  Bʏ\B��B˙�B�{B̏\B�
=B͙�B�(�BΣ�B�33B��
B�Q�B��HB�p�B��B�ffB���BӅB�{Bԣ�B�G�B��B�ffB���BׅB�  B؏\B�33B��
B�ffB��HB�p�B�  B܏\B��B�B�ffB���B߅B�{B�RB�p�B�{B�RB�\)B��B�\B��B��
B�z�B�G�B��
B�z�B��B�B�ffB�
=B�B�z�B�33B��
B�z�B�
=B�B�z�B�33B��
B�z�B�33B��
B�\B�G�B�  B���B�\)B�  B���B�p�B�(�B���B���B�=qB��HB���B�Q�B��B��
C G�C ��C ��CG�C��C
=CffCC{Cp�CC�Cz�C�
C33C�C�
C33C��C��CQ�C�C  C\)C�RC	�C	z�C	��C
(�C
z�C
�
C33C�\C�HC33C�C�HCG�C�\C�C=qC��C��CQ�C��C�CG�C��C  C\)C��C��CG�C�C
=CQ�C��C��CG�C��C  CQ�C��C�CG�C��C�C=qC�C�HC=qC�C��C�Cp�CC
=CQ�C��C  CQ�C��C�
C(�C�C��C{C\)C�RC
=CQ�C�\C�HC=qC�C��C {C \)C �RC!  C!G�C!�C!��C"(�C"p�C"�C"�C#33C#�\C#��C$
=C$Q�C$��C$�C%33C%p�C%�C&  C&Q�C&��C&�HC'�C'ffC'C(
=C(G�C(�C(��C)�C)p�C)�RC)��C*=qC*�\C*�
C+�C+\)C+�C,  C,G�C,�C,��C-(�C-p�C-�C.  C.Q�C.��C.�C/(�C/z�C/�
C0{C0\)C0�C1  C1Q�C1�\C1�HC233C2�\C2�
C3{C3ffC3C4{C4\)C4��C4��C5Q�C5��C5�C633C6�\C6�HC7(�C7z�C7��C8(�C8p�C8�RC9{C9ffC9�RC:  C:Q�C:�C:��C;G�C;��C;��C<=qC<�C<�HC==qC=z�C=�
C>33C>z�C>C?�C?z�C?�RC@
=C@ffC@�RCA  CAG�CA��CA��CBG�CB�\CB�CC=qCC�CC�
CD33CDz�CDCE{CEp�CECF
=CFQ�CF�CG  CG=qCG�\CG�
CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�NG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A���A���A�  A�  A�A�A�1A�A�  A�  A���A�A�%A�A���A���A���A���A���A��`A��;A���Aٺ^AٮA٥�Aٛ�Aٕ�AٍPA�n�A�JAؾwAإ�A�`BA�oAו�A֮A�33AѾwA���A��A���A�ĜA�9XA�|�A�hsA��7A�-A���A�9XA�33A���A�=qA�/A�z�A��+A���A��uA���A�JA�I�A���A�A|�jA{�Ax�\Au�;AshsArȴAqK�Ap-An5?Ah~�Ah$�Ag�Ag�7Ad�\A_��A^jA]K�AZ��AXE�AS�wAQO�AM\)AJZAH~�AFbADr�ABI�A@  A>�!A=&�A;\)A8jA7;dA6^5A6{A5�TA5��A5��A5A5\)A4��A45?A3�A2��A2bNA1�mA1;dA0r�A/�A/dZA/�wA/�TA/��A/\)A/
=A.�/A.��A-�A-C�A-A,bNA+hsA*�HA*v�A*�A)S�A)"�A)A(�A( �A'ƨA'�A'K�A&ȴA&A�A%�-A$��A$�A$��A$�RA$^5A$=qA$bA#A#S�A"�A"�A!�A �A ZA �A�A��A{AO�Av�A��A��A�^AhsAhsA�A�A��A�A1AO�AAĜA�AjA-A�AdZA�\AA�A�A�^A�AXAZA�7AS�A;dA%A��A^5A9XA�#A33AQ�A�
A�hA33A%A�yA��AVA�AK�A�A��A{A��Ax�A\)A
�`A
VA	��A	�
A	��A	x�A��A�Ap�AC�AAn�A�;A�wAXAVA��A��AȴA�RA~�A{A  A�A�wAhsA;dA�An�A�
A��A`BA+A ��A �+A E�A �@��
@��@���@��7@�Ĝ@�
=@�=q@��@�G�@�9X@��m@��w@�+@��!@�n�@�&�@�b@�K�@���@���@���@�Z@�b@�5?@��^@��@�Z@�dZ@�@�@��y@��@�@�p�@�&�@���@�@�A�@��;@�@�;d@旍@��@��#@�^@�@�%@�j@�K�@�V@���@�&�@��/@��D@�1@��@ݺ^@ܓu@���@ۥ�@�|�@��H@ى7@���@�z�@�Q�@��;@׾w@׍P@�;d@���@�v�@�J@ա�@�X@�V@ԣ�@�9X@ӥ�@�
=@ҟ�@�=q@�{@��#@с@�V@�Ĝ@мj@�z�@�bN@�9X@�|�@�+@��y@ΰ!@���@�7L@̼j@��@�C�@ʸR@ɉ7@ț�@ȃ@�z�@�z�@�(�@Ǯ@�dZ@�K�@�V@Ų-@�V@ċD@�j@�Q�@�b@���@öF@�@�V@��h@�x�@�X@��@���@�z�@�I�@��m@���@��+@��7@�`B@�X@���@�A�@��;@��P@�C�@�o@���@���@�5?@���@�7L@��u@�Q�@��@��@�"�@�v�@�$�@��^@��@���@� �@�t�@��@��\@�M�@�$�@��@�X@���@��u@��D@�j@��@���@�t�@�@�~�@�$�@�p�@��`@���@�Z@�(�@�ƨ@�;d@���@��R@�@���@���@�X@�?}@�7L@�Ĝ@�b@��m@�ƨ@�|�@�dZ@�C�@�@�V@��^@�hs@�/@��u@���@���@��y@���@�v�@��@���@��@���@��@�  @��
@��@�C�@���@�ff@�5?@�@��#@���@��7@�O�@��@��@���@�z�@�I�@��@�t�@�+@�
=@��@�v�@�M�@�-@��@��T@���@��@���@���@�I�@�1'@� �@���@���@�t�@�S�@�C�@�o@��@�n�@�{@��@���@�x�@�?}@���@�bN@� �@�b@��@��P@���@���@���@�M�@�-@��@���@���@�`B@�X@�/@���@��u@�A�@�b@��
@���@�t�@�+@��@��+@�^5@�5?@�J@�@��@��^@���@��@�G�@��@��/@���@��D@�Z@� �@���@�C�@���@���@��R@�~�@�@���@�/@���@��D@�Q�@�b@��
@��@�+@��@�o@���@��+@�ff@��#@���@��@�7L@��`@��u@���@��w@�|�@�;d@��H@���@��R@���@��+@�E�@���@��7@�O�@�&�@�V@���@�  @�@��@�@�P@\)@~ff@~$�@~5?@~$�@}��@|�j@|1@{�@{@y��@y��@yx�@y�@xĜ@x �@w|�@w
=@v��@v�@vv�@u�-@t�/@t9X@s��@s�@s33@rn�@q��@pbN@pA�@p1'@p �@o|�@n$�@m�@m@m?}@l�@l�/@lz�@k��@kC�@k@j~�@i��@i��@ihs@h��@h  @g
=@fV@e�T@e`B@dj@d9X@d�@c��@c��@cdZ@c@b��@bn�@b^5@b=q@b�@a�@a��@a��@a��@a��@a��@a�^@a��@a��@a&�@`�9@`r�@`Q�@` �@_�@_l�@_�@^$�@]�-@]p�@\�@\��@\�D@\z�@[��@Z�H@Z��@Y��@Y�7@YX@Y&�@X�`@X�u@XA�@X  @W|�@WK�@W;d@V��@V��@V{@U�@T��@Tj@T9X@S�m@S��@S�@SdZ@S33@R�H@R�@Q��@P��@Pb@O\)@N�R@Nv�@NV@N5?@N{@M�T@Mp�@L�D@L1@Kt�@J�H@J�\@JM�@J=q@J-@I��@I��@I��@I��@I7L@HĜ@HA�@G�@G�w@GK�@G�@F�@F�R@F5?@E��@E`B@D�@D�D@D(�@C�F@Ct�@CC�@C33@Co@C@B�\@BJ@Ahs@@�9@@b@?�w@?�P@?\)@>�y@>ȴ@>��@=�@=?}@<��@<z�@<�@;�
@;�F@;dZ@;C�@;"�@;"�@:�\@:M�@9�#@9x�@8�`@8Ĝ@8bN@7�@7|�@6�y@6�@6V@5�T@5p�@4��@4�j@4�D@4I�@4(�@4�@4�@3�m@3o@2�H@2��@2^5@1�@1��@1��@1hs@1�@0��@0��@0�9@0�u@0r�@0r�@0r�@0b@/�@/�@.�+@-��@-O�@-V@,��@-?}@,��@,Z@,1@+ƨ@+S�@+@*�H@*�@)�^@)hs@(�`@(Q�@'�@'��@'K�@&��@&��@&V@%�T@%�h@%/@$��@$Z@$9X@$1@#�m@$j@$Z@$(�@#ƨ@#ƨ@#dZ@#o@#o@#o@#@"�@"�H@"��@"~�@"�@!��@!�@!�#@!�#@!�^@!7L@!%@ ��@ Ĝ@ ��@ bN@ 1'@�;@�@\)@
=@�+@@�-@��@�@p�@?}@�@��@�j@�@�@��@��@��@�D@j@I�@��@33@o@��@�\@=q@-@=q@J@��@x�@7L@�@��@Ĝ@r�@��@\)@
=@ȴ@�+@v�@v�@v�@{@�@��@��@��@�j@z�@(�@��@�@dZ@C�@"�@�@�H@~�@J@��@�@�^@�7@x�@hs@hs@G�@7L@�@�@&�@�`@�u@�@bN@1'@  @  @�@�@��@|�@;d@
=@
=@��@�y@�@ȴ@��@��@�+@ff@ff@V@5?@$�@�@�T@��@O�@�@V@��@�D@�D@j@(�@1@1@�m@�F@��@dZ@33@
�H@
�!@
n�@
=q@	��@	��@	��@	�7@	hs@	XA��A��A��A��A��A��A��A���A���A���A��A��A���A���A���A���A�  A�A�A���A���A�A�A�%A�A�A�A�A�1A�1A�1A�A�%A�A�1A���A���A�A�A�  A���A�A�  A�  A���A���A���A���A���A���A���A���A���A���A���A�%A�1A�1A�A�A�%A�1A�1A�1A�%A�A�1A�1A�A���A���A�A�1A�A�  A�  A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A�  A���A��A��A��A���A���A���A���A���A�  A�  A�A�  A�A�  A�A�A��A��yA��yA��yA��`A��HA��HA��TA��`A��HA��;A��;A��HA��`A��/A��#A��A��#A��#A���A���AٸRAټjAټjAپwAټjAپwAٸRAټjAپwAټjAٶFAٲ-Aٴ9AٶFAٴ9AٮA٧�A٧�A٩�AٮA٩�A٣�A٥�A٥�A٥�A١�Aٟ�A١�Aٟ�Aٛ�Aٛ�Aٝ�Aٛ�Aٗ�AٓuAٕ�Aٗ�Aٙ�Aٙ�Aٗ�AٓuAُ\AًDAُ\AّhAّhAٍPAى7Aى7AمA�~�A�x�A�r�A�l�A�hsA�hsA�hsA�dZA�`BA�Q�A� �A���A��A��/A��
A���A���A�ĜA���A���AؾwAظRAز-AخAجAة�Aا�Aا�Aأ�Aأ�A؛�AؓuA؇+A�x�A�jA�Q�A�?}A�=qA�=qA�7LA�/A�$�A��A�1A���A���A���A��A��A״9Aש�AדuA�|�A�dZA�G�A��A���A��/A���A֙�AցA�p�A�ZA�C�A�
=AՋDA�v�A��A��AҾwAҰ!A�r�A�ZA��A���A���Aѥ�AуA�Q�A�$�AЇ+A�~�A�|�A�~�A�|�A�~�AЁAЁAЁAЃAЃAЁA�|�A�~�A�|�A�|�A�|�A�~�A�|�A�|�A�r�A�t�A�p�A�jA�ffA�bNA�XA�O�A�G�A�C�A�?}A�9XA�/A�&�A� �A��A�VA�A���A���A��A��A��TA��#A���A�ƨAϺ^A϶FAϴ9Aϲ-Aϰ!Aϧ�Aϝ�AϑhAσA�t�A�hsA�`BA�XA�Q�A�E�A�7LA�&�A��A��A���A�AΝ�AΟ�AΕ�A�|�A�p�A�n�A�p�A�\)A�ZA�\)A�S�A�ZA�XA�=qA�=qA�=qA�/A��A��A�%A��yA��/A��/A��;A��;A��#A���A���A���A���A���A���A͝�A͓uA�l�A�Q�A�ZA�;dA�bA�ĜA�hsA�
=Aˡ�A�bNA��Aʟ�A���A��A�z�A��A�^5AąA�p�A�Q�A�/A���A�`BA�5?A��#A���A�^5A�&�A��A���A�v�A��RA�n�A���A��FA���A���A��PA��7A��A�~�A�z�A�x�A�r�A�bNA�\)A�?}A���A��A��A�`BA�-A�VA���A���A�E�A�&�A��A��+A�5?A���A�%A�O�A�A��yA��^A��7A�JA��A�jA�VA���A��-A�hsA�7LA��mA�1'A��mA��A���A���A�bNA�33A��A���A�ƨA���A��DA�XA�$�A��;A��A�t�A�M�A�  A��wA�jA���A��A��A��^A��A�Q�A�^5A��A���A�XA�$�A�ȴA�G�A�;dA�+A���A�(�A�
=A���A��/A��A��7A�G�A�x�A��/A�I�A��;A��#A���A��DA�I�A��A��A�A�O�A���A�G�A�
=A��-A�~�A�1'A���A��yA�C�A��TA��+A�C�A�
=A��;A��FA���A�dZA�  A��;A���A�33A�O�A��wA�jA��;A�t�A�K�A���A�G�A�  A��mA���A�ƨA��9A���A�K�A�A�z�A�A�1'A���A��-A��A���A�n�A��`A��A�A�A���A�|�A�A���A���A�(�A��A~��A}�#A}|�A}"�A|�A|�!A|��A|z�A|Q�A|�A{�A{`BAz�`Az=qAyx�Ax��Ax�/Ax�AxI�Ax$�Aw�
Aw7LAvjAu�At�HAtZAs�mAs�PAsS�AsG�As?}As?}As7LAs�As%Ar��ArA�Aq�mAq��Aqp�AqC�Aq�Ap��Ap��Ap�\ApM�ApbAo�;AoAo��Aox�Ao/AnffAl��Ak
=Ah��AhbNAhA�Ah5?Ah9XAh1'Ah1'Ah1'Ah-Ah�AhAg��Ag��Ag��Ag�Ag�;Ag��Ag�FAg��Ag�PAg�AghsAg?}Ag33Af��AeC�Act�Aa�Aa`BAaoA`�!A_��A_C�A_VA^��A^�A^��A^r�A^1'A]��A]��A]��A]�A]p�A]&�A\�jA\n�A[�A[/AZ��AZ�AZ9XAY�AY�PAY`BAYS�AX�AW��AV��AT��AT�AS�mAS��AS��AS|�ASp�ASK�AS7LAR�DAQ&�AP9XAO��AO%AN~�AN(�AM��AM;dAL�AL~�AK�FAK"�AJ�9AJn�AJQ�AI��AI�^AI��AIp�AH��AH�jAH�uAH(�AG�-AGO�AFȴAF��AF�AE�
AE��AE��AE�ADȴAD��AD�ADn�ADI�AD1'AD  ACAC|�AB��ABA�AA��AA+A@��A@��A@ffA@I�A@bA?�^A?�A?�hA?x�A?C�A>��A>n�A>^5A>VA>A�A=�TA=�A=x�A<�yA<�+A<v�A<jA<I�A<1'A<A;�A:�RA9S�A8�`A8�jA8��A8�A8A�A8bA7ƨA7��A7�A7G�A7%A6�`A6�RA6��A6�+A6ffA6M�A6=qA6 �A6JA5�A5�A6bA6(�A6E�A6(�A6A5�A5�;A5�;A5ƨA5�-A5��A5��A5��A5��A5�-A5�wA5ƨA5�A6JA6bA6A5��A5�A5�#A5ƨA5A5A5�-A5��A5��A5�PA5dZA5?}A533A5&�A5�A5VA5%A4��A4�yA4�`A4�jA4��A4jA4I�A4(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                   A��A��A���A���A�  A�  A�A�A�1A�A�  A�  A���A�A�%A�A���A���A���A���A���A��`A��;A���Aٺ^AٮA٥�Aٛ�Aٕ�AٍPA�n�A�JAؾwAإ�A�`BA�oAו�A֮A�33AѾwA���A��A���A�ĜA�9XA�|�A�hsA��7A�-A���A�9XA�33A���A�=qA�/A�z�A��+A���A��uA���A�JA�I�A���A�A|�jA{�Ax�\Au�;AshsArȴAqK�Ap-An5?Ah~�Ah$�Ag�Ag�7Ad�\A_��A^jA]K�AZ��AXE�AS�wAQO�AM\)AJZAH~�AFbADr�ABI�A@  A>�!A=&�A;\)A8jA7;dA6^5A6{A5�TA5��A5��A5A5\)A4��A45?A3�A2��A2bNA1�mA1;dA0r�A/�A/dZA/�wA/�TA/��A/\)A/
=A.�/A.��A-�A-C�A-A,bNA+hsA*�HA*v�A*�A)S�A)"�A)A(�A( �A'ƨA'�A'K�A&ȴA&A�A%�-A$��A$�A$��A$�RA$^5A$=qA$bA#A#S�A"�A"�A!�A �A ZA �A�A��A{AO�Av�A��A��A�^AhsAhsA�A�A��A�A1AO�AAĜA�AjA-A�AdZA�\AA�A�A�^A�AXAZA�7AS�A;dA%A��A^5A9XA�#A33AQ�A�
A�hA33A%A�yA��AVA�AK�A�A��A{A��Ax�A\)A
�`A
VA	��A	�
A	��A	x�A��A�Ap�AC�AAn�A�;A�wAXAVA��A��AȴA�RA~�A{A  A�A�wAhsA;dA�An�A�
A��A`BA+A ��A �+A E�A �@��
@��@���@��7@�Ĝ@�
=@�=q@��@�G�@�9X@��m@��w@�+@��!@�n�@�&�@�b@�K�@���@���@���@�Z@�b@�5?@��^@��@�Z@�dZ@�@�@��y@��@�@�p�@�&�@���@�@�A�@��;@�@�;d@旍@��@��#@�^@�@�%@�j@�K�@�V@���@�&�@��/@��D@�1@��@ݺ^@ܓu@���@ۥ�@�|�@��H@ى7@���@�z�@�Q�@��;@׾w@׍P@�;d@���@�v�@�J@ա�@�X@�V@ԣ�@�9X@ӥ�@�
=@ҟ�@�=q@�{@��#@с@�V@�Ĝ@мj@�z�@�bN@�9X@�|�@�+@��y@ΰ!@���@�7L@̼j@��@�C�@ʸR@ɉ7@ț�@ȃ@�z�@�z�@�(�@Ǯ@�dZ@�K�@�V@Ų-@�V@ċD@�j@�Q�@�b@���@öF@�@�V@��h@�x�@�X@��@���@�z�@�I�@��m@���@��+@��7@�`B@�X@���@�A�@��;@��P@�C�@�o@���@���@�5?@���@�7L@��u@�Q�@��@��@�"�@�v�@�$�@��^@��@���@� �@�t�@��@��\@�M�@�$�@��@�X@���@��u@��D@�j@��@���@�t�@�@�~�@�$�@�p�@��`@���@�Z@�(�@�ƨ@�;d@���@��R@�@���@���@�X@�?}@�7L@�Ĝ@�b@��m@�ƨ@�|�@�dZ@�C�@�@�V@��^@�hs@�/@��u@���@���@��y@���@�v�@��@���@��@���@��@�  @��
@��@�C�@���@�ff@�5?@�@��#@���@��7@�O�@��@��@���@�z�@�I�@��@�t�@�+@�
=@��@�v�@�M�@�-@��@��T@���@��@���@���@�I�@�1'@� �@���@���@�t�@�S�@�C�@�o@��@�n�@�{@��@���@�x�@�?}@���@�bN@� �@�b@��@��P@���@���@���@�M�@�-@��@���@���@�`B@�X@�/@���@��u@�A�@�b@��
@���@�t�@�+@��@��+@�^5@�5?@�J@�@��@��^@���@��@�G�@��@��/@���@��D@�Z@� �@���@�C�@���@���@��R@�~�@�@���@�/@���@��D@�Q�@�b@��
@��@�+@��@�o@���@��+@�ff@��#@���@��@�7L@��`@��u@���@��w@�|�@�;d@��H@���@��R@���@��+@�E�@���@��7@�O�@�&�@�V@���@�  @�@��@�@�P@\)@~ff@~$�@~5?@~$�@}��@|�j@|1@{�@{@y��@y��@yx�@y�@xĜ@x �@w|�@w
=@v��@v�@vv�@u�-@t�/@t9X@s��@s�@s33@rn�@q��@pbN@pA�@p1'@p �@o|�@n$�@m�@m@m?}@l�@l�/@lz�@k��@kC�@k@j~�@i��@i��@ihs@h��@h  @g
=@fV@e�T@e`B@dj@d9X@d�@c��@c��@cdZ@c@b��@bn�@b^5@b=q@b�@a�@a��@a��@a��@a��@a��@a�^@a��@a��@a&�@`�9@`r�@`Q�@` �@_�@_l�@_�@^$�@]�-@]p�@\�@\��@\�D@\z�@[��@Z�H@Z��@Y��@Y�7@YX@Y&�@X�`@X�u@XA�@X  @W|�@WK�@W;d@V��@V��@V{@U�@T��@Tj@T9X@S�m@S��@S�@SdZ@S33@R�H@R�@Q��@P��@Pb@O\)@N�R@Nv�@NV@N5?@N{@M�T@Mp�@L�D@L1@Kt�@J�H@J�\@JM�@J=q@J-@I��@I��@I��@I��@I7L@HĜ@HA�@G�@G�w@GK�@G�@F�@F�R@F5?@E��@E`B@D�@D�D@D(�@C�F@Ct�@CC�@C33@Co@C@B�\@BJ@Ahs@@�9@@b@?�w@?�P@?\)@>�y@>ȴ@>��@=�@=?}@<��@<z�@<�@;�
@;�F@;dZ@;C�@;"�@;"�@:�\@:M�@9�#@9x�@8�`@8Ĝ@8bN@7�@7|�@6�y@6�@6V@5�T@5p�@4��@4�j@4�D@4I�@4(�@4�@4�@3�m@3o@2�H@2��@2^5@1�@1��@1��@1hs@1�@0��@0��@0�9@0�u@0r�@0r�@0r�@0b@/�@/�@.�+@-��@-O�@-V@,��@-?}@,��@,Z@,1@+ƨ@+S�@+@*�H@*�@)�^@)hs@(�`@(Q�@'�@'��@'K�@&��@&��@&V@%�T@%�h@%/@$��@$Z@$9X@$1@#�m@$j@$Z@$(�@#ƨ@#ƨ@#dZ@#o@#o@#o@#@"�@"�H@"��@"~�@"�@!��@!�@!�#@!�#@!�^@!7L@!%@ ��@ Ĝ@ ��@ bN@ 1'@�;@�@\)@
=@�+@@�-@��@�@p�@?}@�@��@�j@�@�@��@��@��@�D@j@I�@��@33@o@��@�\@=q@-@=q@J@��@x�@7L@�@��@Ĝ@r�@��@\)@
=@ȴ@�+@v�@v�@v�@{@�@��@��@��@�j@z�@(�@��@�@dZ@C�@"�@�@�H@~�@J@��@�@�^@�7@x�@hs@hs@G�@7L@�@�@&�@�`@�u@�@bN@1'@  @  @�@�@��@|�@;d@
=@
=@��@�y@�@ȴ@��@��@�+@ff@ff@V@5?@$�@�@�T@��@O�@�@V@��@�D@�D@j@(�@1@1@�m@�F@��@dZ@33@
�H@
�!@
n�@
=q@	��@	��@	��@	�7@	hsG�O�A��A��A��A��A��A��A��A���A���A���A��A��A���A���A���A���A�  A�A�A���A���A�A�A�%A�A�A�A�A�1A�1A�1A�A�%A�A�1A���A���A�A�A�  A���A�A�  A�  A���A���A���A���A���A���A���A���A���A���A���A�%A�1A�1A�A�A�%A�1A�1A�1A�%A�A�1A�1A�A���A���A�A�1A�A�  A�  A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A�  A���A��A��A��A���A���A���A���A���A�  A�  A�A�  A�A�  A�A�A��A��yA��yA��yA��`A��HA��HA��TA��`A��HA��;A��;A��HA��`A��/A��#A��A��#A��#A���A���AٸRAټjAټjAپwAټjAپwAٸRAټjAپwAټjAٶFAٲ-Aٴ9AٶFAٴ9AٮA٧�A٧�A٩�AٮA٩�A٣�A٥�A٥�A٥�A١�Aٟ�A١�Aٟ�Aٛ�Aٛ�Aٝ�Aٛ�Aٗ�AٓuAٕ�Aٗ�Aٙ�Aٙ�Aٗ�AٓuAُ\AًDAُ\AّhAّhAٍPAى7Aى7AمA�~�A�x�A�r�A�l�A�hsA�hsA�hsA�dZA�`BA�Q�A� �A���A��A��/A��
A���A���A�ĜA���A���AؾwAظRAز-AخAجAة�Aا�Aا�Aأ�Aأ�A؛�AؓuA؇+A�x�A�jA�Q�A�?}A�=qA�=qA�7LA�/A�$�A��A�1A���A���A���A��A��A״9Aש�AדuA�|�A�dZA�G�A��A���A��/A���A֙�AցA�p�A�ZA�C�A�
=AՋDA�v�A��A��AҾwAҰ!A�r�A�ZA��A���A���Aѥ�AуA�Q�A�$�AЇ+A�~�A�|�A�~�A�|�A�~�AЁAЁAЁAЃAЃAЁA�|�A�~�A�|�A�|�A�|�A�~�A�|�A�|�A�r�A�t�A�p�A�jA�ffA�bNA�XA�O�A�G�A�C�A�?}A�9XA�/A�&�A� �A��A�VA�A���A���A��A��A��TA��#A���A�ƨAϺ^A϶FAϴ9Aϲ-Aϰ!Aϧ�Aϝ�AϑhAσA�t�A�hsA�`BA�XA�Q�A�E�A�7LA�&�A��A��A���A�AΝ�AΟ�AΕ�A�|�A�p�A�n�A�p�A�\)A�ZA�\)A�S�A�ZA�XA�=qA�=qA�=qA�/A��A��A�%A��yA��/A��/A��;A��;A��#A���A���A���A���A���A���A͝�A͓uA�l�A�Q�A�ZA�;dA�bA�ĜA�hsA�
=Aˡ�A�bNA��Aʟ�A���A��A�z�A��A�^5AąA�p�A�Q�A�/A���A�`BA�5?A��#A���A�^5A�&�A��A���A�v�A��RA�n�A���A��FA���A���A��PA��7A��A�~�A�z�A�x�A�r�A�bNA�\)A�?}A���A��A��A�`BA�-A�VA���A���A�E�A�&�A��A��+A�5?A���A�%A�O�A�A��yA��^A��7A�JA��A�jA�VA���A��-A�hsA�7LA��mA�1'A��mA��A���A���A�bNA�33A��A���A�ƨA���A��DA�XA�$�A��;A��A�t�A�M�A�  A��wA�jA���A��A��A��^A��A�Q�A�^5A��A���A�XA�$�A�ȴA�G�A�;dA�+A���A�(�A�
=A���A��/A��A��7A�G�A�x�A��/A�I�A��;A��#A���A��DA�I�A��A��A�A�O�A���A�G�A�
=A��-A�~�A�1'A���A��yA�C�A��TA��+A�C�A�
=A��;A��FA���A�dZA�  A��;A���A�33A�O�A��wA�jA��;A�t�A�K�A���A�G�A�  A��mA���A�ƨA��9A���A�K�A�A�z�A�A�1'A���A��-A��A���A�n�A��`A��A�A�A���A�|�A�A���A���A�(�A��A~��A}�#A}|�A}"�A|�A|�!A|��A|z�A|Q�A|�A{�A{`BAz�`Az=qAyx�Ax��Ax�/Ax�AxI�Ax$�Aw�
Aw7LAvjAu�At�HAtZAs�mAs�PAsS�AsG�As?}As?}As7LAs�As%Ar��ArA�Aq�mAq��Aqp�AqC�Aq�Ap��Ap��Ap�\ApM�ApbAo�;AoAo��Aox�Ao/AnffAl��Ak
=Ah��AhbNAhA�Ah5?Ah9XAh1'Ah1'Ah1'Ah-Ah�AhAg��Ag��Ag��Ag�Ag�;Ag��Ag�FAg��Ag�PAg�AghsAg?}Ag33Af��AeC�Act�Aa�Aa`BAaoA`�!A_��A_C�A_VA^��A^�A^��A^r�A^1'A]��A]��A]��A]�A]p�A]&�A\�jA\n�A[�A[/AZ��AZ�AZ9XAY�AY�PAY`BAYS�AX�AW��AV��AT��AT�AS�mAS��AS��AS|�ASp�ASK�AS7LAR�DAQ&�AP9XAO��AO%AN~�AN(�AM��AM;dAL�AL~�AK�FAK"�AJ�9AJn�AJQ�AI��AI�^AI��AIp�AH��AH�jAH�uAH(�AG�-AGO�AFȴAF��AF�AE�
AE��AE��AE�ADȴAD��AD�ADn�ADI�AD1'AD  ACAC|�AB��ABA�AA��AA+A@��A@��A@ffA@I�A@bA?�^A?�A?�hA?x�A?C�A>��A>n�A>^5A>VA>A�A=�TA=�A=x�A<�yA<�+A<v�A<jA<I�A<1'A<A;�A:�RA9S�A8�`A8�jA8��A8�A8A�A8bA7ƨA7��A7�A7G�A7%A6�`A6�RA6��A6�+A6ffA6M�A6=qA6 �A6JA5�A5�A6bA6(�A6E�A6(�A6A5�A5�;A5�;A5ƨA5�-A5��A5��A5��A5��A5�-A5�wA5ƨA5�A6JA6bA6A5��A5�A5�#A5ƨA5A5A5�-A5��A5��A5�PA5dZA5?}A533A5&�A5�A5VA5%A4��A4�yA4�`A4�jA4��A4jA4I�A4(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                   ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BH�BH�BH�BHKBG�BHKBHKBH�BHBH�BH�BHBG�BHBH�BH�BG�BGzBGEBG�BHBF�BF�BF?BE�BEmBE9BEBD�BEmBHBH�BFtBDgBB�B@�BC�BK)Bt�B��B	�B	�.B	��B
B
EmB
S�B
\]B
p�B
��B
��B
��B
�wB
RTB
,qB
�B	��B	��B	�EB	��B	��B	�B	�B	~�B	�PB	�SB	�GB	u�B	�%B	{JB	yrB	u�B	n�B	m�B	PHB	B�B	?}B	:*B	:�B	�B	�B	xB	�B	�B�B�%B�;B��B�&B��B�%B	 �B�;B�mB�`B	�B	,qB	>B	N<B	iB	u�B	{B	��B	��B	�~B	��B	��B	ʌB	�jB	�B	�aB	�yB	ݘB	�TB	��B
�B
�B
&�B
3�B
:�B
<B
<�B
C-B
B[B
A�B
D�B
I�B
F�B
EmB
IRB
L�B
OvB
WsB
W?B
ZQB
Z�B
Y�B
ZB
ZQB
Z�B
\]B
\]B
\)B
[�B
[�B
\]B
\)B
[WB
\)B
Z�B
[�B
Z�B
[�B
X�B
XyB
XB
W
B
V9B
R�B
R�B
Q�B
S&B
T,B
Y�B
\�B
_;B
`�B
`�B
aB
`BB
_�B
_B
]/B
Z�B
ZQB
X�B
W?B
Y�B
\�B
YB
V�B
V�B
T�B
S�B
R�B
P}B
NB
MB
L�B
MB
MB
K)B
J�B
M�B
NpB
J�B
G�B
H�B
K^B
K�B
LdB
L�B
L0B
K�B
J�B
HKB
G�B
D�B
D�B
DgB
EB
E9B
EB
C�B
C�B
C�B
B�B
DgB
B�B
?�B
?HB
?HB
@�B
>B
>B
>B
=<B
<6B
;�B
;�B
;dB
;�B
:�B
9�B
9�B
9XB
8�B
7�B
7�B
6zB
4nB
2�B
2�B
1�B
1�B
1[B
3hB
2�B
1�B
1'B
1'B
/�B
.IB
.�B
,B
+�B
+�B
+�B
)�B
)�B
)�B
($B
'�B
'�B
%zB
$B
!�B
 'B
xB
�B
=B
B
YB
YB
�B
�B
�B
{B
�B
�B
�B
�B
�B
SB
SB
�B
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
SB
�B
�B
 B
~B

=B

�B

�B
B

�B
	B
+B
�B
�B
SB
�B
�B
GB
�B
GB
�B
�B
{B
B
B
�B
B
�B
SB
�B
YB
�B
fB
	7B
	B

�B

�B
xB
B
B
�B
�B
�B
�B
~B
�B
xB

�B
�B

	B
	7B
	lB
	B

=B
	�B
	�B
	B
DB
�B
	�B
	7B
�B
�B
�B
�B
1B
�B
B

	B
	�B
	�B
	lB
	�B

	B

rB
B
B
�B
B
~B
�B
"B
�B
VB
\B
\B
(B
(B
(B
.B
.B
bB
�B
�B
�B
bB
4B
oB
�B
:B
oB
:B
�B
�B
hB
�B
4B
�B
4B
�B
�B
.B
B
�B
YB
$B
�B
$B
B
$B
�B
�B
�B
�B
�B
�B
B
�B
uB
{B
B
FB
{B
B
�B
B
FB
FB
�B
FB
�B
�B
�B
MB
B
{B
FB
�B
B
MB
SB
�B
�B
�B
$B
�B
+B
�B
�B
�B
1B
�B
	B
kB
�B
7B
�B
�B
�B
�B
CB
B
=B
�B
kB
	B
qB
�B
qB
B
B
�B
CB
CB
CB
CB
B
 \B
!bB
!�B
!�B
!�B
!�B
!�B
"4B
#nB
#�B
#�B
#nB
#nB
#nB
#:B
#:B
#�B
#nB
$@B
$@B
$tB
$@B
$B
$�B
%zB
$�B
%FB
%�B
%�B
%�B
%zB
&B
&B
%�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
($B
(�B
(�B
(�B
(�B
(�B
)*B
(�B
(�B
)_B
)_B
(�B
)�B
)�B
*�B
*�B
*�B
+B
,B
,�B
,�B
-�B
-CB
-B
,�B
-�B
.B
-�B
.B
.IB
.�B
.�B
.�B
/OB
/�B
/OB
.�B
/�B
/OB
/�B
0UB
0UB
/�B
0�B
0�B
0�B
1�B
1'B
2-B
2aB
33B
2�B
2�B
2�B
2�B
3�B
4�B
4�B
5B
4�B
4�B
5�B
6FB
6B
6B
5�B
5�B
6B
7B
6zB
6FB
6B
6zB
6�B
6�B
6�B
7LB
8�B
7�B
7�B
7�B
7�B
8�B
9�B
:�B
:�B
:^B
:�B
;�B
<6B
<6B
<6B
;�B
;�B
;�B
<B
=B
<�B
<jB
<6B
=�B
?B
>�B
?B
?}B
?}B
?HB
?}B
@OB
@�B
@�B
A�B
B'B
A�B
A�B
C-B
C�B
DgB
D�B
EB
E�B
FtB
F?B
F?B
F?B
F�B
FtB
GB
GB
GB
GB
GB
GzB
GB
GzB
G�B
GEB
GzB
GzB
GzB
G�B
GzB
HKB
H�B
H�B
H�B
IB
H�B
IRB
IRB
J�B
JXB
J�B
K^B
K)B
J�B
J�B
L0B
LdB
LdB
M�B
M6B
MjB
M�B
M�B
M�B
M�B
N<B
N�B
NpB
N<B
N�B
NpB
OBB
O�B
O�B
PB
PB
PHB
P}B
P}B
PHB
P}B
P}B
Q�B
QNB
R�B
R B
S�B
S�B
S[B
S�B
S[B
S�B
S�B
T,B
U2B
T�B
U�B
VB
VB
V9B
V9B
V9B
VmB
V�B
V�B
VmB
W
B
W
B
W�B
W�B
W�B
XEB
XB
XyB
XEB
X�B
YB
YB
YB
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
ZQB
Z�B
Z�B
[WB
\)B
\]B
\�B
\�B
\�B
]/B
\�B
\�B
]�B
]�B
^�B
^jB
^�B
^�B
^�B
_B
_B
_B
^�B
_�B
_pB
_�B
_�B
`vB
`BB
`�B
`�B
aHB
a�B
aHB
bB
bB
b�B
b�B
cTB
cTB
c�B
c�B
c�B
c�B
d&B
d�B
dZB
d�B
e,B
e`B
e`B
e`B
e�B
f�B
f�B
f�B
gmB
g�B
gmB
gmB
g8B
g�B
gmB
h
B
iDB
iyB
i�B
jB
j�B
m]B
n�B
n�B
n�B
o5B
oiB
o5B
oiB
o B
n/B
m�B
m�B
m�B
m]B
m�B
m]B
m�B
m�B
m�B
ncB
m�B
n�B
oiB
o B
o B
oiB
o�B
r|B
sMB
s�B
s�B
s�B
s�B
tB
tB
tTB
t�B
u%B
u%B
uZB
u�B
v+B
v`B
v`B
v`B
v+B
v+B
w2B
w2B
w2B
wfB
wfB
w2B
wfB
w�B
w�B
xB
w�B
w�B
w�B
x8B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y	B
y>B
yrB
y�B
y�B
y�B
y�B
z�B
z�B
{B
{B
{B
{�B
{B
{�B
{�B
{�B
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|PB
|PB
|PB
|B
|�B
|�B
|�B
|�B
|�B
|�B
}VB
}�B
~]B
}�B
~]B
~]B
~�B
~]B
~�B
~�B
~]B
~�B
~�B
~�B
~�B
.B
�B
�B
��B
��B
�;B
�oB
�oB
�B
�AB
�uB
�uB
�B
�GB
�B
�{B
��B
�B
��B
��B
�SB
��B
�B
��B
��B
�%B
�%B
��B
��B
��B
��B
��B
��B
�YB
�YB
��B
��B
��B
��B
�_B
�fB
�fB
��B
��B
��B
�B
�fB
�fB
��B
��B
��B
�	B
�=B
�	B
�rB
�=B
�	B
�	B
�B
��B
�	B
��BI�BI�BH�BGzBHKBIBI�BH�BHBHBIRBJ�BHKBG�BG�BGzBH�BG�BGzBIBIRBHKBGEBGzBIRBIBHBGzBGEBG�BH�BI�BG�BI�BGEBK�BH�BHBGEBH�BK�BH�BHKBGEBHBH�BG�BGzBG�BH�BH�BGBFtBH�BI�BFBG�BGzBH�BIBI�BHBG�BHBH�BI�BH�BHKBIRBHKBH�BGEBGBH�BIRBI�BHKBHBH�BGEBF�BFtBF�BH�BGBH�BF?BGEBG�BGzBF�BE�BIRBIRBGEBF?BF�BF�BH�BIBIBG�BE�BG�BIBIBG�BFBGEBK�BFtBG�BEBH�BGEBGzBF?BE9BFBHBF�BEmBE9BG�BGEBG�BEmBEmBHKBDgBIBE9BF�BF?BC�BD�BGBE9BDgBEBGBFtBDgBEBD3BE�BFtBF?BDgBC�BE�BFtBD�BDgBEBE�BE9BE9BEBE�BEmBDgBD3BF?BE�BEmBD3BC�BC�BD3BE9BF?BF�BEBDgBD�BE9BE�BFBF�BFtBG�BG�BHKBIBHKBGzBF�BGzBIRBOBJ�BE�BF�BE9BGEBH�BGEBFBD�BFBGBGBF?BD�BC�BC�BD3BD�BD�BD�BB�BC�BB�BDgBD�BC�B@�B@�B@B?�BA�B@�BB�BA�B?�B=�B=�BB'BA�BA�BB�BDgBD�BF�BH�BI�BK�BIRBQ�BK)BK�BK^BM�BS�BaHBy>B�SBcB�SB��B�	B��B�MB�B��B��B�B��B�}B�vB�B�B��B�B��BںB��B��BںB��B�)B�/B�dB�jB�B�B�B��B��B�"B�/B�B�B�B��B�B��B��B��B��B��B�B	�B	B	1B	uB	�B	kB	�B	�B	_B	�B	�B	�B	�B	CB	�B	�B	CB	B	�B	"�B	'B	1[B	9�B	D�B	R�B	\]B	c�B	hsB	k�B	p;B	oiB	u%B	t�B	y�B	}�B	{�B	~�B	��B	��B	�1B	��B	��B	��B	�PB	��B	��B	�JB	��B	� B	�\B	�oB	��B	�B	�@B	�{B	�SB	�MB	�B	�B	��B	�B	�uB	�B	��B	�B	�B	��B	�B	�-B	��B	��B	�YB	��B	��B	��B	��B	�nB	��B	��B	��B	�WB	�AB	�B	�HB	��B
l"B
oB
�B
�B
�B
�B
B
�B
SB
OB
SB
?}B
@B
<6B
A�B
o B
]/B
V�B
WsB
W
B
XB
U�B
U2B
T,B
T�B
R�B
R�B
Q�B
P�B
Q�B
XEB
v+B
YKB
U2B
W?B
UgB
V9B
[WB
b�B
WsB
ZB
�;B
p�B
oiB
jB
v�B
l�B
s�B
zxB
~�B
��B
�(B
��B
�JB
�~B
�B
��B
��B
��B
�1B
��B
��B
��B
бB
�B
��B
�aB
�B
ǮB
��B
��B
��B
��B
��B
�}B
�9B
�kB
��B
��B
�LB
��B
бB
��B
�~B
g�B
i�B
rB
T,B
N�B
K�B
F�B
RTB
DgB
3�B
5tB
D�B
2�B
($B
#B
%zB
$�B
$B
�B
+�B
t�B
DB	�)B	�B	�cB	�B	�)B	��B	��B	�B	�+B	��B	�WB	�B	�B	��B	��B	�B	�"B	� B	� B	�&B	�B	ܒB	�B	��B	�HB	�HB	��B	�&B	�<B	��B	چB	��B	��B	�B	��B	��B	�mB	˒B	��B	��B	�nB	��B	�-B	�IB	�<B	��B	��B	��B	�IB	�MB	�B	�mB	�IB	�	B	��B	��B	cB	|PB	{B	{JB	yrB	�tB	��B	��B	��B	��B	�GB	��B	��B	�SB	�{B	��B	�AB	��B	�B	�iB	��B	��B	�B	tB	u�B	xB	s�B	s�B	x�B	��B	�	B	��B	��B	��B	��B	��B	{JB	zxB	w2B	v+B	v`B	w�B	u�B	z�B	}�B	|�B	y>B	v�B	u�B	s�B	r|B	q�B	qvB	qAB	p�B	l"B	i�B	g�B	c�B	gmB	o5B	p�B	u�B	|B	K)B	D�B	FB	DgB	C�B	B[B	@�B	A�B	A�B	EmB	@OB	?B	?HB	>BB	?}B	=�B	;�B	9XB	8B	<B	8RB	8�B	33B	E�B	:^B	`�B	'�B	)�B	_B	!�B	%�B	�B	B	1B	�B	{B	�B	�B	�B	B	~B	7B	�B	VB	�B	 'B	"�B	'�B	 �B	�B	�B	=B	_B	�B	(B	B	B	,qB	�B��B�xB�B�(B�8B�B��B�WB		7B	�B�"B�2B�B�&BݘB�pB��B��B�jB�&B��B�&BܒB�WB� B��B��B�B��B�;B��B�B�2B�
B�"B�KB�B�KB�B�B	B��B��B�B�TB��B��B��B�PB�B	_B	 �B	
	B�PB	B�B�PB�B��B�QB��B�yB�2B�cB�B�B�B�B�B�;B��B�B	�B��B��B��B�TB�%B�+B	_B	
rB	�B	 \B	"�B	&�B	(�B	33B	6FB	@B	8B	9�B	>�B	A B	D3B	C-B	G�B	H�B	K^B	M�B	QNB	X�B	c B	d&B	b�B	a�B	iyB	qvB	uZB	v�B	t�B	t�B	tTB	v`B	v�B	v`B	yrB	x�B	zB	}VB	�4B	�B	��B	��B	�oB	��B	�CB	�OB	��B	��B	�CB	��B	�!B	��B	�=B	�~B	�VB	�~B	�qB	�xB	�!B	�!B	�!B	�\B	�@B	�B	�0B	�kB	�!B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                   BH�BHfBH�BG�BG�BH1BHKBHfBHKBH�BH�BH1BG�BG�BH�BH�BG�BGzBG+BG�BH�BGBGBF�BE�BE�BEmBEBEBF?BJ	BJ	BGBE�BD�BC{BI�BX�B��B�B	WB	��B	�OB
1B
U2B
a-B
r�B
��B
��B
�B
�B
ևB
cTB
D�B
;B	��B	��B	�B	چB	�B	̘B	�dB	�:B	�SB	�DB	�^B	HB	��B	}�B	~(B	y�B	vB	~�B	R:B	DB	A�B	D3B	I7B	#�B	�B	$�B	'�B	'RB	B	�B�yB�B�qB��B��B	�B�B�CB��B	�B	0�B	AB	OvB	i�B	v�B	z�B	��B	�VB	�!B	�`B	�lB	˒B	��B	��B	��B	�=B	�pB	�,B	��B
(B
B
(XB
4�B
;0B
<�B
?�B
EmB
C{B
DB
G�B
K^B
H1B
F�B
K�B
MPB
PB
YB
X�B
[�B
[�B
Z�B
[�B
\CB
\�B
^�B
\�B
\]B
[�B
\�B
\�B
\�B
\�B
]�B
\�B
^B
]B
^�B
YeB
YB
ZB
Y�B
X�B
UgB
U�B
S�B
S�B
T,B
Z�B
\�B
`BB
a�B
a�B
a�B
a�B
bNB
`'B
^B
[=B
[=B
Y�B
XEB
\B
_;B
Z�B
W�B
W�B
UB
UgB
U�B
S@B
N�B
MjB
M�B
NVB
NB
K�B
LdB
P.B
QhB
L�B
IB
J=B
K�B
LB
MPB
NB
M�B
NB
K^B
I�B
J=B
F�B
EB
D�B
F�B
GEB
F?B
DgB
D�B
DMB
D�B
G�B
DgB
@�B
@OB
AUB
B�B
>�B
?}B
?B
>B
<jB
;�B
;�B
<6B
=B
;JB
:DB
:�B
:�B
9>B
9$B
9rB
8�B
5ZB
3�B
3�B
3MB
2�B
2aB
4B
3�B
2�B
2�B
33B
1'B
1[B
0UB
,�B
-B
-wB
,=B
*eB
*�B
*B
(�B
)�B
)�B
&�B
%,B
#�B
!�B
�B
xB
OB
�B
�B
�B
?B
$B
�B
�B
MB
B
�B
mB
SB
�B

B
?B
SB
mB
�B
yB
$B
9B
mB
�B
$B
�B
yB
�B
�B
$B
$B
YB
�B
mB
B
�B

�B
B
�B
pB
0B
	�B
�B
�B
?B
�B
�B
B
3B
�B
B
gB
GB
MB
�B
3B
�B
�B
mB
�B
B
B
�B
�B
	RB
	�B
B
DB
�B
�B
�B
<B
B
.B
�B
B
B
�B
�B
�B

=B
	RB
	�B
	�B
B

rB

	B

�B
~B

	B

�B
	�B
	B
	B
�B
�B
	�B

#B
dB

XB
	�B

	B

	B

�B

rB
)B
�B
B
�B
jB
�B
�B
\B
BB
�B
�B
�B
�B
�B
�B
4B
B
�B
NB
�B
4B
hB
oB
&B
�B
[B
[B
@B
�B
[B
oB
 B
�B
NB
TB
�B
4B
bB
oB
�B
�B
sB
�B
+B
�B
yB
�B
yB
YB
YB
sB
�B
�B
{B
�B
�B
{B
�B
�B
FB
�B
SB
�B
�B
aB
{B
�B
FB
�B
mB
�B
�B
�B

B
�B
�B
�B
B
mB
sB
+B
�B
B
1B
B
7B
�B
�B
�B
�B
B
�B
B
B
CB
IB
�B
�B
�B
	B
#B
�B
�B
�B
�B
�B
]B
)B
�B
xB
xB
�B
OB
 �B
!�B
!�B
!�B
"4B
"hB
"NB
"�B
#�B
$@B
$ZB
$@B
$&B
#�B
#�B
#�B
$&B
$@B
%B
$�B
$�B
$�B
$�B
%�B
%�B
%,B
%�B
&2B
&B
&2B
%�B
&�B
&2B
&LB
'8B
'8B
'�B
'�B
'�B
(XB
(XB
(�B
)_B
)_B
(�B
)B
)B
)DB
(�B
)_B
)�B
)�B
)_B
)�B
*B
+6B
+B
+B
+�B
,�B
-�B
-CB
-�B
-wB
-�B
-�B
.�B
/ B
.�B
.�B
.�B
/iB
/5B
/OB
/�B
/�B
/iB
/�B
0!B
/�B
0�B
0�B
0�B
0�B
1'B
1AB
2B
2B
1�B
2�B
3B
3hB
2�B
2�B
2�B
3�B
4�B
5%B
5B
5ZB
5B
5tB
7LB
6`B
6+B
6+B
6B
6+B
6�B
7LB
6zB
6`B
6zB
7�B
7fB
7fB
7LB
8RB
8�B
8B
7�B
8RB
8�B
9XB
:^B
;B
:�B
:�B
;�B
<�B
<�B
<�B
<�B
<6B
<jB
<�B
="B
=<B
<�B
<�B
<�B
>�B
?HB
>�B
?�B
?�B
?�B
?�B
@ B
@�B
AB
AB
BAB
B[B
B'B
B�B
C�B
D�B
EB
ESB
E�B
F�B
F�B
FYB
FtB
F�B
F�B
F�B
G_B
G_B
G+B
G+B
G+B
G�B
G+B
GzB
G�B
GEB
GzB
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I7B
I�B
J=B
K)B
J�B
KB
K�B
KDB
KB
K�B
L�B
L�B
M6B
M�B
MjB
M�B
M�B
M�B
N"B
N"B
N�B
N�B
N�B
N�B
OB
OB
P.B
P.B
PHB
PHB
PbB
P�B
P�B
P�B
P}B
P�B
Q4B
RB
RB
S�B
R�B
T,B
S�B
S�B
S�B
S�B
S�B
TB
T�B
U�B
UgB
VB
VSB
VSB
VSB
VSB
VmB
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
XEB
XyB
X_B
X�B
X�B
YKB
YeB
YB
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[qB
[�B
\B
\�B
\�B
\�B
\�B
\�B
]IB
]/B
]dB
^�B
^jB
^�B
^�B
_!B
_B
_!B
_!B
_!B
_!B
_VB
_�B
_�B
`BB
`vB
`�B
`�B
aB
abB
a�B
a�B
a�B
b�B
b�B
c B
c:B
c�B
c�B
c�B
c�B
c�B
dB
d�B
d�B
d�B
eB
e�B
ezB
e�B
e�B
fLB
f�B
f�B
gB
g�B
g�B
gmB
gmB
g�B
g�B
g�B
i*B
jB
i�B
j0B
jKB
j�B
m�B
o5B
oB
oB
o�B
o�B
oiB
p!B
oiB
n�B
n}B
n�B
n/B
m�B
m�B
m�B
m�B
nIB
ncB
n�B
ncB
oB
o�B
oB
o5B
o�B
oiB
r�B
s�B
s�B
s�B
tB
t9B
tB
tB
tnB
uB
u?B
u?B
u�B
v`B
vFB
vzB
vzB
v`B
v`B
v�B
wfB
wLB
wfB
w�B
w�B
wfB
w�B
w�B
w�B
xRB
xRB
xB
xB
xRB
xRB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
y>B
yrB
y�B
y�B
zB
zxB
{B
z�B
{dB
{�B
{�B
{�B
{B
{�B
|B
|B
|�B
|jB
|�B
|�B
|�B
}VB
}<B
}B
}B
|�B
|jB
|PB
|PB
|�B
|�B
}<B
}VB
|�B
}B
}B
}�B
~BB
~wB
~B
~wB
~wB
~�B
~wB
~�B
.B
~wB
~�B
~�B
~�B
B
HB
�B
�B
��B
��B
�;B
�oB
��B
�[B
�[B
��B
��B
�GB
�GB
�-B
��B
��B
�MB
��B
��B
�SB
��B
�9B
��B
�B
�?B
�?B
�B
�B
��B
�B
��B
��B
��B
�tB
��B
�B
��B
��B
��B
��B
�fB
��B
�KB
��B
�B
��B
��B
�B
��B
�	B
�XB
�rB
�XB
��B
��B
�=B
�=B
�)B
�B
�#G�O�BI�BI�BH�BGzBHKBIBI�BH�BHBHBIRBJ�BHKBG�BG�BGzBH�BG�BGzBIBIRBHKBGEBGzBIRBIBHBGzBGEBG�BH�BI�BG�BI�BGEBK�BH�BHBGEBH�BK�BH�BHKBGEBHBH�BG�BGzBG�BH�BH�BGBFtBH�BI�BFBG�BGzBH�BIBI�BHBG�BHBH�BI�BH�BHKBIRBHKBH�BGEBGBH�BIRBI�BHKBHBH�BGEBF�BFtBF�BH�BGBH�BF?BGEBG�BGzBF�BE�BIRBIRBGEBF?BF�BF�BH�BIBIBG�BE�BG�BIBIBG�BFBGEBK�BFtBG�BEBH�BGEBGzBF?BE9BFBHBF�BEmBE9BG�BGEBG�BEmBEmBHKBDgBIBE9BF�BF?BC�BD�BGBE9BDgBEBGBFtBDgBEBD3BE�BFtBF?BDgBC�BE�BFtBD�BDgBEBE�BE9BE9BEBE�BEmBDgBD3BF?BE�BEmBD3BC�BC�BD3BE9BF?BF�BEBDgBD�BE9BE�BFBF�BFtBG�BG�BHKBIBHKBGzBF�BGzBIRBOBJ�BE�BF�BE9BGEBH�BGEBFBD�BFBGBGBF?BD�BC�BC�BD3BD�BD�BD�BB�BC�BB�BDgBD�BC�B@�B@�B@B?�BA�B@�BB�BA�B?�B=�B=�BB'BA�BA�BB�BDgBD�BF�BH�BI�BK�BIRBQ�BK)BK�BK^BM�BS�BaHBy>B�SBcB�SB��B�	B��B�MB�B��B��B�B��B�}B�vB�B�B��B�B��BںB��B��BںB��B�)B�/B�dB�jB�B�B�B��B��B�"B�/B�B�B�B��B�B��B��B��B��B��B�B	�B	B	1B	uB	�B	kB	�B	�B	_B	�B	�B	�B	�B	CB	�B	�B	CB	B	�B	"�B	'B	1[B	9�B	D�B	R�B	\]B	c�B	hsB	k�B	p;B	oiB	u%B	t�B	y�B	}�B	{�B	~�B	��B	��B	�1B	��B	��B	��B	�PB	��B	��B	�JB	��B	� B	�\B	�oB	��B	�B	�@B	�{B	�SB	�MB	�B	�B	��B	�B	�uB	�B	��B	�B	�B	��B	�B	�-B	��B	��B	�YB	��B	��B	��B	��B	�nB	��B	��B	��B	�WB	�AB	�B	�HB	��B
l"B
oB
�B
�B
�B
�B
B
�B
SB
OB
SB
?}B
@B
<6B
A�B
o B
]/B
V�B
WsB
W
B
XB
U�B
U2B
T,B
T�B
R�B
R�B
Q�B
P�B
Q�B
XEB
v+B
YKB
U2B
W?B
UgB
V9B
[WB
b�B
WsB
ZB
�;B
p�B
oiB
jB
v�B
l�B
s�B
zxB
~�B
��B
�(B
��B
�JB
�~B
�B
��B
��B
��B
�1B
��B
��B
��B
бB
�B
��B
�aB
�B
ǮB
��B
��B
��B
��B
��B
�}B
�9B
�kB
��B
��B
�LB
��B
бB
��B
�~B
g�B
i�B
rB
T,B
N�B
K�B
F�B
RTB
DgB
3�B
5tB
D�B
2�B
($B
#B
%zB
$�B
$B
�B
+�B
t�B
DB	�)B	�B	�cB	�B	�)B	��B	��B	�B	�+B	��B	�WB	�B	�B	��B	��B	�B	�"B	� B	� B	�&B	�B	ܒB	�B	��B	�HB	�HB	��B	�&B	�<B	��B	چB	��B	��B	�B	��B	��B	�mB	˒B	��B	��B	�nB	��B	�-B	�IB	�<B	��B	��B	��B	�IB	�MB	�B	�mB	�IB	�	B	��B	��B	cB	|PB	{B	{JB	yrB	�tB	��B	��B	��B	��B	�GB	��B	��B	�SB	�{B	��B	�AB	��B	�B	�iB	��B	��B	�B	tB	u�B	xB	s�B	s�B	x�B	��B	�	B	��B	��B	��B	��B	��B	{JB	zxB	w2B	v+B	v`B	w�B	u�B	z�B	}�B	|�B	y>B	v�B	u�B	s�B	r|B	q�B	qvB	qAB	p�B	l"B	i�B	g�B	c�B	gmB	o5B	p�B	u�B	|B	K)B	D�B	FB	DgB	C�B	B[B	@�B	A�B	A�B	EmB	@OB	?B	?HB	>BB	?}B	=�B	;�B	9XB	8B	<B	8RB	8�B	33B	E�B	:^B	`�B	'�B	)�B	_B	!�B	%�B	�B	B	1B	�B	{B	�B	�B	�B	B	~B	7B	�B	VB	�B	 'B	"�B	'�B	 �B	�B	�B	=B	_B	�B	(B	B	B	,qB	�B��B�xB�B�(B�8B�B��B�WB		7B	�B�"B�2B�B�&BݘB�pB��B��B�jB�&B��B�&BܒB�WB� B��B��B�B��B�;B��B�B�2B�
B�"B�KB�B�KB�B�B	B��B��B�B�TB��B��B��B�PB�B	_B	 �B	
	B�PB	B�B�PB�B��B�QB��B�yB�2B�cB�B�B�B�B�B�;B��B�B	�B��B��B��B�TB�%B�+B	_B	
rB	�B	 \B	"�B	&�B	(�B	33B	6FB	@B	8B	9�B	>�B	A B	D3B	C-B	G�B	H�B	K^B	M�B	QNB	X�B	c B	d&B	b�B	a�B	iyB	qvB	uZB	v�B	t�B	t�B	tTB	v`B	v�B	v`B	yrB	x�B	zB	}VB	�4B	�B	��B	��B	�oB	��B	�CB	�OB	��B	��B	�CB	��B	�!B	��B	�=B	�~B	�VB	�~B	�qB	�xB	�!B	�!B	�!B	�\B	�@B	�B	�0B	�kB	�!B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<l2<b�b<fO<?>�<|��=,�f<��<�1�<fO<�-6<��<��-<��<�)�=&�<���<�XP<��<��<�O�<��<�<L}�<�&�<��r<�a!<)��<#�
<#�
<-��<#�
<#�
<#�
<#�
<#�
<�O�<#�
<#�
<#�
<7��<{f�<#�
<#�
<#�
<*L�<mh<(��<_S�<:^<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<2��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018051310232520180513102325IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018052317032520180523170325QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018052317032520180523170325QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550720190521075507IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                
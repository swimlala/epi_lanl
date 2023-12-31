CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-11-20T21:49:47Z creation; 2021-03-26T17:00:53Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20191120214947  20210326170159  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7836_008777_020                 7836_008777_020                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @��uv��@��uv��11  @��u���'@��u���'@<�C�k&�@<�C�k&��d�s��d�s�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@   @@  @z�H@�  @�  @�G�A   A��A ��A,(�A@��A`��A�  A�  A�Q�A�Q�A�  AϮA�  A�  A��B(�BQ�B(�B   B(  B/�B7�B?�
BG�BP  BX  B_�
Bg�Bo�Bw�B�  B�{B�(�B�(�B�{B�{B��B��B�  B�{B�{B�  B��
B��B�  B�{B�{B�  B�  B�(�B��B��
B�{B�=qB�(�B�(�B�(�B�(�B�(�B�=qB�=qB�=qC {C�C{C
=C��C	�HC��C  C  C  C  C
=C
=C  C��C  C 
=C"  C#��C%��C'��C*  C,  C.  C0  C2  C3��C6  C8  C:
=C<{C=��C?�CA�CC�CE��CH
=CJ  CL  CN  CO��CR
=CT
=CV
=CX
=CZ
=C\
=C]��C_��Cb{Cd
=Cf  Ch
=Cj
=Cl
=Cn  Co��Cq��Cs��Cv  Cx
=Cz  C{��C}��C�  C�  C���C�  C�  C���C���C���C�C�
=C�  C�  C�  C�  C�C�C�C�  C���C�  C�C�C�  C���C��C���C���C���C���C���C�  C�  C�  C�  C�  C�C�\C�C�  C�
=C�C���C��C���C�  C�  C�  C���C�  C���C���C�  C�
=C�C�C�  C�  C�  C���C���C�  C�C�C�
=C�C�C���C���C���C�  C�C�C�C�C�  C���C�  C�  C�  C���C�C�C���C�C�  C���C���C�  C���C���C�C�  C�  C�C�  C���C���C�  C�C�C���C�C�\C�
=C���C���C�C�C���C���C���C���C�  C�  C�C�  C�C�C�  C�  C�C�C�  C�  C�C�
=C���C���C���D }qD  Dz�D  D�D
=D��D�D��DD��D  D� D  D�D  Dz�D�qD	� D	��D
xRD
��D� D�qD}qD  D�D�Dz�D�qD��D  D� D�qD�DD�D�D��D  D� D  D��D  D}qD  D� D�D� D��D}qD  D}qD�qD}qD�qD}qD  Dz�D�qD� D  D��D   D z�D ��D!� D"�D"� D"�qD#� D$�D$��D%  D%}qD%��D&� D'�D'� D'�qD(}qD)  D)��D*�D*� D+�D+��D,D,� D,�qD-��D.  D.� D.�qD/}qD0�D0��D1�D1}qD2  D2�D3D3� D3��D4z�D5  D5}qD5��D6��D7D7��D8  D8� D9  D9� D:�D:��D;  D;��D<D<� D=  D=� D=�qD>}qD>�qD?� D?�qD@z�D@�qDA}qDB�DB��DC  DC��DD�DD��DE  DE��DF�DF�DGDG��DH  DH� DI  DI� DJ�DJ}qDJ�qDK}qDK�qDL}qDM  DM}qDM�qDN� DO  DO� DP�DP��DQ�DQ��DR�DR� DR�qDS��DT�DT� DT�qDU� DVDV� DW�DW� DW�qDX}qDY  DY��DZ  DZ}qD[  D[� D\  D\��D]D]�D^�D^}qD^�qD_� D`�D`� Da  Da� Db  Db� Db��Dcz�Dd  Dd��De  De� Df�Df}qDf�qDg� Dh  Dh}qDi�Di��Dj�Dj� Dk�Dk��Dl  Dl}qDl��Dm� Dn  Dn� Do�Do��DpDp� Dq  Dq��Dq�qDr}qDs�Ds��Ds�qDtxRDt��Du}qDu�qDv}qDv�qDw��DxDx�Dy�Dy�Dz�Dz� D{�D{��D|D|��D}  D}� D~�D~� D  D� D�  D�@ D��HD��HD��D�AHD�~�D���D�HD�B�D��HD���D�  D�AHD��HD��HD�  D�@ D�� D���D�  D�@ D�� D�� D�  D�B�D�� D���D�  D�>�D�~�D��HD��D�AHD�~�D���D���D�AHD��HD�� D�  D�>�D�~�D�� D�  D�@ D��HD���D��qD�AHD��HD���D�  D�AHD�� D��qD�  D�AHD���D�D���D�@ D�� D�� D�HD�AHD�~�D���D�  D�AHD�~�D���D���D�AHD���D��qD�  D�B�D���D�� D���D�@ D��HD�� D�  D�@ D�� D�D��D�AHD�� D��HD��D�AHD��HD���D�  D�AHD�~�D��HD��qD�<)D�~�D��HD�  D�=qD�}qD���D�  D�@ D�~�D���D�HD�B�D��HD��HD�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�AHD���D�� D��D�@ D��HD�� D���D�>�D�� D��HD��qD�<)D�� D���D�HD�@ D�� D�D��D�>�D�}qD�� D�HD�@ D��HD��qD�  D�C�D��HD�� D�  D�=qD��HD�D�HD�>�D�� D�� D�HD�AHD�~�D�� D���D�AHD�}qD�� D���D�@ D��HD���D�  D�@ D��HD�� D�  D�>�D�� D�� D�  D�AHD��HD���D���D�AHD��HD�D�  D�>�D�}qD�� D���D�AHD�� D�D��D�@ D�~�D���D��D�AHD�� D�� D�HD�B�D�~�D���D���D�=qD�~�D�� D�  D�@ D�~�D�� D�  D�=qD�~�D��HD�  D�@ D�~�D��HD�HD�>�D�}qD��qD���D�=qD�� D��HD�  D�@ D�� D�� D�  D�@ D�� D���D�  D�>�D�� D��HD�  D�AHD�� D���D���D�@ D D½qD�  D�>�DÁHD�D��D�@ D�}qD�� D�HD�B�Dŀ D�� D�HD�>�D�~�D�� D�  D�>�D�}qD�� D���D�AHDȁHD��HD�HD�AHD�~�D�� D��qD�>�Dʀ D�� D���D�>�DˁHD�D�HD�>�D�}qD̾�D���D�=qD�~�D�� D�  D�AHD�~�D�� D���D�>�Dπ D�� D�HD�@ DЀ D�D��D�@ D�~�D��HD�  D�>�DҀ D�� D�HD�>�D�~�DӽqD���D�>�D�~�D��HD�HD�@ DՀ D�� D�HD�AHDր D�� D���D�>�D׀ D�� D���D�>�D؀ D��HD��D�@ D�}qD�� D�  D�@ Dڃ�D�D�HD�>�DہHD��HD��D�@ D܂�Dܾ�D�  D�>�D�}qD��HD�HD�B�DށHD�� D�HD�@ D߁HD�� D��qD�>�D���D��HD��qD�AHD�~�D�qD���D�AHD�~�D��HD�  D�>�D� D㾸D�  D�@ D�~�D�� D�  D�AHD� D�� D�  D�B�D� D�qD���D�AHD�HD�� D�  D�B�D� D辸D�  D�@ D�}qD�� D�HD�@ D�}qD�qD�  D�>�D�~�D뾸D���D�@ D�}qD�qD�  D�@ D�~�D�� D��qD�@ D� D��HD�HD�>�D�HD�)D�  D�@ D�� D�D��D�AHD� D��HD���D�=qD�~�D�� D�HD�@ D�~�D�D�  D�AHD�~�D��HD�HD�B�D��HD���D�  D�AHD�� D��qD�HD�AHD�� D��HD�  D�>�D��HD�� D�  D�@ D�~�D���D���D�AHD�o\?#�
?L��?�  ?�=q?�33?Ǯ?�(�?��?��H@�@��@#�
@0��@5@8Q�@E�@Q�@Y��@fff@s33@z�H@�  @��@�=q@�33@�(�@��
@�=q@�{@�@���@��
@�{@�@޸R@�ff@�\)@�
=@�p�A33AQ�A��A�A�A(�A   A%�A(��A+�A1�A6ffA;�A@  AE�AI��AL��AQG�AU�AY��A^�RAc�
Aj=qAn�RAs33Aw
=Az�HA~{A���A�33A�A�Q�A��HA��A�\)A��A��
A�A�
=A���A��A�{A�Q�A��HA���A�\)A���A��HA��A�
=A���A�33A��A��A��A�z�A��RA�G�A��
A��A�\)A�G�A��HA��AθRA���A��
A�{A�Q�A�=qA�(�A�{A߮AᙚA�A��A�A�=qA�z�A�
=A���A��HA���A�ffA�  A�=qA�(�A��RB Q�B��B�RB�B��B��B�RB�B��B	��B
�RB�BG�B{B\)BQ�Bp�B�\B�
B��B��B�\B�B��BG�B�\B�Bz�BB�HB   B!�B!B"�RB$  B$��B%�B&�RB'�B(z�B)B+
=B,Q�B-p�B.=qB/33B0  B1�B1�B3
=B4(�B5G�B6�\B7�
B8��B9�B;33B<(�B=�B>=qB?33B@(�BAG�BB=qBC33BDQ�BEG�BF�RBG�
BI�BJ=qBK
=BL  BL��BM�BN�HBO�
BP��BQ��BR�RBT  BT��BV{BW\)BXQ�BYG�BZ=qB[33B\Q�B]�B^ffB_33B`z�BaG�BbffBc\)Bdz�Be��Bf�\Bg�Bh��Bi�Bj�HBl(�BmG�BnffBo�Bp��BqBs
=Bt(�BuG�Bv�\Bw�
Bx��Bz=qB{�B|��B}B
=B�  B��RB�G�B��
B��\B�33B�B�ffB�
=B�B�z�B��B��
B�ffB��B��
B�z�B��B�B�ffB���B���B�(�B��RB�\)B��B��\B�G�B��B���B�G�B��B��\B�G�B��B���B�G�B��B�z�B�
=B��B�=qB��HB�p�B�(�B���B��B�{B���B��B�=qB���B�\)B�  B��\B�33B�B�ffB��B�B�ffB��B��
B��\B��B��B�=qB���B�\)B�  B��RB�\)B�  B���B�\)B�  B���B��B�B�=qB��HB�p�B�{B��HB��B�=qB���B��B�{B��RB�G�B��B�ffB���B��B�Q�B���B��B�Q�B���B��B�{B���B�33B�B\B�33B��Bģ�B�33B�B�Q�B��HBǙ�B�Q�B���BɮB�Q�B���B˙�B�(�B���B�\)B��BΏ\B�G�B��BУ�B�\)B�  Bң�B�33BӮB�Q�B�
=BծB�z�B�33B�B�Q�B��HB�\)B�  BڸRB�p�B�(�B���B�p�B��B�ffB�
=B�B�z�B�33B�  B�\B��B�B�(�B��HB噚B�ffB�
=B癚B�=qB�RB�G�B�  B�RB�\)B�{B���B�\)B�  B�\B�
=B�B�ffB��B��
B�\B�33B�B�=qB���B���B�ffB��B�B�Q�B��HB�p�B�  B���B�\)B�{B���B�p�B�  B��\B��B��C =qC �\C ��C=qC�C��C�C�C�HC=qC�C��C{Cp�C��C33C�\C�
C�CffC�RC�C�C�
C�CffC�RC	{C	z�C	�HC
(�C
ffC
�C{Cp�C�
C(�CffC�C
=Cp�C�
C{CQ�C�C{Cz�CC
=CQ�CC(�Cp�C�RC  Cp�C�
C�CffC�C{Cp�C�
C(�Cp�C�RC�Cz�C�
C�CffC�RC(�C�C�
C�CffC�
C33C�C�
C�C�C�C=qC�\C��C=qC��C�C(�Cp�C��C�CQ�Cz�C��C {C \)C �\C �C ��C!G�C!�\C!�RC!�HC"�C"p�C"�RC"�HC#
=C#\)C#�C#�HC${C$=qC$�\C$�HC%{C%=qC%�\C%�
C&
=C&33C&z�C&C'{C'=qC'ffC'�C(
=C(=qC(ffC(�C(��C)=qC)z�C)��C)�C*=qC*�C*�RC*�HC+�C+p�C+C,  C,33C,ffC,��C,��C-G�C-z�C-�C-�HC.33C.�C.C.��C/(�C/z�C/�
C0
=C033C0z�C0��C1{C1G�C1z�C1C2�C2Q�C2z�C2��C3�C3\)C3��C3C4�C4p�C4��C4��C5�C5p�C5�RC5�HC6�C6p�C6��C6��C7(�C7z�C7��C8
=C833C8z�C8�
C9{C9G�C9�\C9�HC:�C:Q�C:�\C:�C;33C;\)C;��C<  C<G�C<p�C<��C<��C=Q�C=�C=�C>  C>\)C>�\C>C?
=C?\)C?��C?��C@{C@ffC@��C@�HCA{CAffCA�RCA�CB{CBffCB�RCC  CC33CCffCC�RCD
=CD=qCDz�CD�CE
=CEQ�CEz�CE�RCF
=CF\)CF�\CFCG{CGffCG�\CG��CH�CHffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                              ?�=q@   @@  @z�H@�  @�  @�G�A   A��A ��A,(�A@��A`��A�  A�  A�Q�A�Q�A�  AϮA�  A�  A��B(�BQ�B(�B   B(  B/�B7�B?�
BG�BP  BX  B_�
Bg�Bo�Bw�B�  B�{B�(�B�(�B�{B�{B��B��B�  B�{B�{B�  B��
B��B�  B�{B�{B�  B�  B�(�B��B��
B�{B�=qB�(�B�(�B�(�B�(�B�(�B�=qB�=qB�=qC {C�C{C
=C��C	�HC��C  C  C  C  C
=C
=C  C��C  C 
=C"  C#��C%��C'��C*  C,  C.  C0  C2  C3��C6  C8  C:
=C<{C=��C?�CA�CC�CE��CH
=CJ  CL  CN  CO��CR
=CT
=CV
=CX
=CZ
=C\
=C]��C_��Cb{Cd
=Cf  Ch
=Cj
=Cl
=Cn  Co��Cq��Cs��Cv  Cx
=Cz  C{��C}��C�  C�  C���C�  C�  C���C���C���C�C�
=C�  C�  C�  C�  C�C�C�C�  C���C�  C�C�C�  C���C��C���C���C���C���C���C�  C�  C�  C�  C�  C�C�\C�C�  C�
=C�C���C��C���C�  C�  C�  C���C�  C���C���C�  C�
=C�C�C�  C�  C�  C���C���C�  C�C�C�
=C�C�C���C���C���C�  C�C�C�C�C�  C���C�  C�  C�  C���C�C�C���C�C�  C���C���C�  C���C���C�C�  C�  C�C�  C���C���C�  C�C�C���C�C�\C�
=C���C���C�C�C���C���C���C���C�  C�  C�C�  C�C�C�  C�  C�C�C�  C�  C�C�
=C���C���C���D }qD  Dz�D  D�D
=D��D�D��DD��D  D� D  D�D  Dz�D�qD	� D	��D
xRD
��D� D�qD}qD  D�D�Dz�D�qD��D  D� D�qD�DD�D�D��D  D� D  D��D  D}qD  D� D�D� D��D}qD  D}qD�qD}qD�qD}qD  Dz�D�qD� D  D��D   D z�D ��D!� D"�D"� D"�qD#� D$�D$��D%  D%}qD%��D&� D'�D'� D'�qD(}qD)  D)��D*�D*� D+�D+��D,D,� D,�qD-��D.  D.� D.�qD/}qD0�D0��D1�D1}qD2  D2�D3D3� D3��D4z�D5  D5}qD5��D6��D7D7��D8  D8� D9  D9� D:�D:��D;  D;��D<D<� D=  D=� D=�qD>}qD>�qD?� D?�qD@z�D@�qDA}qDB�DB��DC  DC��DD�DD��DE  DE��DF�DF�DGDG��DH  DH� DI  DI� DJ�DJ}qDJ�qDK}qDK�qDL}qDM  DM}qDM�qDN� DO  DO� DP�DP��DQ�DQ��DR�DR� DR�qDS��DT�DT� DT�qDU� DVDV� DW�DW� DW�qDX}qDY  DY��DZ  DZ}qD[  D[� D\  D\��D]D]�D^�D^}qD^�qD_� D`�D`� Da  Da� Db  Db� Db��Dcz�Dd  Dd��De  De� Df�Df}qDf�qDg� Dh  Dh}qDi�Di��Dj�Dj� Dk�Dk��Dl  Dl}qDl��Dm� Dn  Dn� Do�Do��DpDp� Dq  Dq��Dq�qDr}qDs�Ds��Ds�qDtxRDt��Du}qDu�qDv}qDv�qDw��DxDx�Dy�Dy�Dz�Dz� D{�D{��D|D|��D}  D}� D~�D~� D  D� D�  D�@ D��HD��HD��D�AHD�~�D���D�HD�B�D��HD���D�  D�AHD��HD��HD�  D�@ D�� D���D�  D�@ D�� D�� D�  D�B�D�� D���D�  D�>�D�~�D��HD��D�AHD�~�D���D���D�AHD��HD�� D�  D�>�D�~�D�� D�  D�@ D��HD���D��qD�AHD��HD���D�  D�AHD�� D��qD�  D�AHD���D�D���D�@ D�� D�� D�HD�AHD�~�D���D�  D�AHD�~�D���D���D�AHD���D��qD�  D�B�D���D�� D���D�@ D��HD�� D�  D�@ D�� D�D��D�AHD�� D��HD��D�AHD��HD���D�  D�AHD�~�D��HD��qD�<)D�~�D��HD�  D�=qD�}qD���D�  D�@ D�~�D���D�HD�B�D��HD��HD�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�AHD���D�� D��D�@ D��HD�� D���D�>�D�� D��HD��qD�<)D�� D���D�HD�@ D�� D�D��D�>�D�}qD�� D�HD�@ D��HD��qD�  D�C�D��HD�� D�  D�=qD��HD�D�HD�>�D�� D�� D�HD�AHD�~�D�� D���D�AHD�}qD�� D���D�@ D��HD���D�  D�@ D��HD�� D�  D�>�D�� D�� D�  D�AHD��HD���D���D�AHD��HD�D�  D�>�D�}qD�� D���D�AHD�� D�D��D�@ D�~�D���D��D�AHD�� D�� D�HD�B�D�~�D���D���D�=qD�~�D�� D�  D�@ D�~�D�� D�  D�=qD�~�D��HD�  D�@ D�~�D��HD�HD�>�D�}qD��qD���D�=qD�� D��HD�  D�@ D�� D�� D�  D�@ D�� D���D�  D�>�D�� D��HD�  D�AHD�� D���D���D�@ D D½qD�  D�>�DÁHD�D��D�@ D�}qD�� D�HD�B�Dŀ D�� D�HD�>�D�~�D�� D�  D�>�D�}qD�� D���D�AHDȁHD��HD�HD�AHD�~�D�� D��qD�>�Dʀ D�� D���D�>�DˁHD�D�HD�>�D�}qD̾�D���D�=qD�~�D�� D�  D�AHD�~�D�� D���D�>�Dπ D�� D�HD�@ DЀ D�D��D�@ D�~�D��HD�  D�>�DҀ D�� D�HD�>�D�~�DӽqD���D�>�D�~�D��HD�HD�@ DՀ D�� D�HD�AHDր D�� D���D�>�D׀ D�� D���D�>�D؀ D��HD��D�@ D�}qD�� D�  D�@ Dڃ�D�D�HD�>�DہHD��HD��D�@ D܂�Dܾ�D�  D�>�D�}qD��HD�HD�B�DށHD�� D�HD�@ D߁HD�� D��qD�>�D���D��HD��qD�AHD�~�D�qD���D�AHD�~�D��HD�  D�>�D� D㾸D�  D�@ D�~�D�� D�  D�AHD� D�� D�  D�B�D� D�qD���D�AHD�HD�� D�  D�B�D� D辸D�  D�@ D�}qD�� D�HD�@ D�}qD�qD�  D�>�D�~�D뾸D���D�@ D�}qD�qD�  D�@ D�~�D�� D��qD�@ D� D��HD�HD�>�D�HD�)D�  D�@ D�� D�D��D�AHD� D��HD���D�=qD�~�D�� D�HD�@ D�~�D�D�  D�AHD�~�D��HD�HD�B�D��HD���D�  D�AHD�� D��qD�HD�AHD�� D��HD�  D�>�D��HD�� D�  D�@ D�~�D���D���D�AHG�O�?#�
?L��?�  ?�=q?�33?Ǯ?�(�?��?��H@�@��@#�
@0��@5@8Q�@E�@Q�@Y��@fff@s33@z�H@�  @��@�=q@�33@�(�@��
@�=q@�{@�@���@��
@�{@�@޸R@�ff@�\)@�
=@�p�A33AQ�A��A�A�A(�A   A%�A(��A+�A1�A6ffA;�A@  AE�AI��AL��AQG�AU�AY��A^�RAc�
Aj=qAn�RAs33Aw
=Az�HA~{A���A�33A�A�Q�A��HA��A�\)A��A��
A�A�
=A���A��A�{A�Q�A��HA���A�\)A���A��HA��A�
=A���A�33A��A��A��A�z�A��RA�G�A��
A��A�\)A�G�A��HA��AθRA���A��
A�{A�Q�A�=qA�(�A�{A߮AᙚA�A��A�A�=qA�z�A�
=A���A��HA���A�ffA�  A�=qA�(�A��RB Q�B��B�RB�B��B��B�RB�B��B	��B
�RB�BG�B{B\)BQ�Bp�B�\B�
B��B��B�\B�B��BG�B�\B�Bz�BB�HB   B!�B!B"�RB$  B$��B%�B&�RB'�B(z�B)B+
=B,Q�B-p�B.=qB/33B0  B1�B1�B3
=B4(�B5G�B6�\B7�
B8��B9�B;33B<(�B=�B>=qB?33B@(�BAG�BB=qBC33BDQ�BEG�BF�RBG�
BI�BJ=qBK
=BL  BL��BM�BN�HBO�
BP��BQ��BR�RBT  BT��BV{BW\)BXQ�BYG�BZ=qB[33B\Q�B]�B^ffB_33B`z�BaG�BbffBc\)Bdz�Be��Bf�\Bg�Bh��Bi�Bj�HBl(�BmG�BnffBo�Bp��BqBs
=Bt(�BuG�Bv�\Bw�
Bx��Bz=qB{�B|��B}B
=B�  B��RB�G�B��
B��\B�33B�B�ffB�
=B�B�z�B��B��
B�ffB��B��
B�z�B��B�B�ffB���B���B�(�B��RB�\)B��B��\B�G�B��B���B�G�B��B��\B�G�B��B���B�G�B��B�z�B�
=B��B�=qB��HB�p�B�(�B���B��B�{B���B��B�=qB���B�\)B�  B��\B�33B�B�ffB��B�B�ffB��B��
B��\B��B��B�=qB���B�\)B�  B��RB�\)B�  B���B�\)B�  B���B��B�B�=qB��HB�p�B�{B��HB��B�=qB���B��B�{B��RB�G�B��B�ffB���B��B�Q�B���B��B�Q�B���B��B�{B���B�33B�B\B�33B��Bģ�B�33B�B�Q�B��HBǙ�B�Q�B���BɮB�Q�B���B˙�B�(�B���B�\)B��BΏ\B�G�B��BУ�B�\)B�  Bң�B�33BӮB�Q�B�
=BծB�z�B�33B�B�Q�B��HB�\)B�  BڸRB�p�B�(�B���B�p�B��B�ffB�
=B�B�z�B�33B�  B�\B��B�B�(�B��HB噚B�ffB�
=B癚B�=qB�RB�G�B�  B�RB�\)B�{B���B�\)B�  B�\B�
=B�B�ffB��B��
B�\B�33B�B�=qB���B���B�ffB��B�B�Q�B��HB�p�B�  B���B�\)B�{B���B�p�B�  B��\B��B��C =qC �\C ��C=qC�C��C�C�C�HC=qC�C��C{Cp�C��C33C�\C�
C�CffC�RC�C�C�
C�CffC�RC	{C	z�C	�HC
(�C
ffC
�C{Cp�C�
C(�CffC�C
=Cp�C�
C{CQ�C�C{Cz�CC
=CQ�CC(�Cp�C�RC  Cp�C�
C�CffC�C{Cp�C�
C(�Cp�C�RC�Cz�C�
C�CffC�RC(�C�C�
C�CffC�
C33C�C�
C�C�C�C=qC�\C��C=qC��C�C(�Cp�C��C�CQ�Cz�C��C {C \)C �\C �C ��C!G�C!�\C!�RC!�HC"�C"p�C"�RC"�HC#
=C#\)C#�C#�HC${C$=qC$�\C$�HC%{C%=qC%�\C%�
C&
=C&33C&z�C&C'{C'=qC'ffC'�C(
=C(=qC(ffC(�C(��C)=qC)z�C)��C)�C*=qC*�C*�RC*�HC+�C+p�C+C,  C,33C,ffC,��C,��C-G�C-z�C-�C-�HC.33C.�C.C.��C/(�C/z�C/�
C0
=C033C0z�C0��C1{C1G�C1z�C1C2�C2Q�C2z�C2��C3�C3\)C3��C3C4�C4p�C4��C4��C5�C5p�C5�RC5�HC6�C6p�C6��C6��C7(�C7z�C7��C8
=C833C8z�C8�
C9{C9G�C9�\C9�HC:�C:Q�C:�\C:�C;33C;\)C;��C<  C<G�C<p�C<��C<��C=Q�C=�C=�C>  C>\)C>�\C>C?
=C?\)C?��C?��C@{C@ffC@��C@�HCA{CAffCA�RCA�CB{CBffCB�RCC  CC33CCffCC�RCD
=CD=qCDz�CD�CE
=CEQ�CEz�CE�RCF
=CF\)CF�\CFCG{CGffCG�\CG��CH�CHffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                              @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A���A���A��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�A���A���A�A�
=A�A���A��uA���A��+A���A���A���A�/A�bNA��uA�1'A���A���A�S�A��-A� �A�bNA�+A��9A��A�9XA��A�-A���A��9A��uA�E�A���A��wA�&�A��A���A��A��A��A�%A~bNA|5?A{?}Az��Az{Ay�FAy�AwƨAvI�Au�7As`BAp�9Ao�mAnȴAm�Al��AlZAk�Ak��Aj1Ai;dAh{Af��Ae�mAe��Ad��Aa�#A`��A_|�A^E�A]�A\�!A\9XA[��A[�AYAXA�AV~�AVjAV=qAU��ATjAT�AS�FARA�AQ33APȴAP �AOG�AN �ALĜAL5?AK�TAJ��AI��AH��AH1AGO�AF��AEƨAC��AB�DAA��A@�A@~�A@$�A?�
A?\)A=��A=`BA="�A;C�A:-A9XA8��A8v�A7��A6��A5x�A4�/A49XA3A3hsA2��A1�;A0�A0-A/\)A.��A.{A-�A,=qA+hsA+�A*��A*^5A)�;A(n�A'�7A'&�A&��A&VA%�TA$��A#ƨA"ȴA"bNA"JA!�^A!hsA!K�A �/A�A;dA�\A1'A�#A&�A��A��A�RA�A5?A�At�Av�A-A��A��A�jAz�A9XA��A|�AhsAK�A�9A �A�A"�A�TA�A$�A�A$�AȴAbNA=qA�#AZAoA
bA	��AA�A�-AS�A�An�A9XAA��A��A�A?}A��A�A ��@��@���@�7L@�=q@��P@�V@�F@�R@�p�@���@�7L@�b@�+@陚@���@�  @��y@�%@�D@�r�@�Z@�b@��`@���@��@��T@�z�@�\)@��y@���@�-@ّh@׶F@ղ-@�r�@�1@ӕ�@��y@�ff@���@���@� �@�^5@��/@��m@�~�@�G�@ȣ�@�j@�33@�%@ÍP@��D@��@�S�@�v�@���@�/@���@���@�r�@�1'@��
@���@��@�J@���@�S�@���@�Z@�S�@��@��@�O�@�/@��9@�bN@�A�@��P@��@��T@�?}@���@���@� �@�$�@��D@�b@��;@��@�+@���@��@�1'@��y@�$�@��^@�hs@��9@�r�@�Q�@�1@���@��@���@��@���@�/@�bN@��w@�\)@�33@��y@���@�J@�x�@��@��9@���@�bN@�dZ@�@��R@�$�@�`B@��`@���@�A�@���@���@�S�@�"�@���@�M�@�{@��T@���@�hs@�/@��/@��D@�A�@�b@��
@�dZ@��@��H@���@�ff@�-@�@���@��7@�7L@���@��/@��u@�1@���@��\@�=q@�{@��@���@��@�X@�&�@�%@��@��@�Q�@��m@��P@�@��+@�E�@��@�J@��@��^@�x�@�X@�7L@���@��@�9X@��m@�ƨ@��@�l�@�33@��@��!@�V@�V@��T@��7@�p�@�X@�?}@�7L@�%@���@��`@��j@�r�@l�@~��@~ȴ@~�+@~E�@}�@|�@|(�@{�
@z��@z^5@y��@y7L@xbN@xb@w�;@w\)@v��@vȴ@vv�@u�T@u�@t�@t��@t�j@t�D@tZ@t1@s��@s"�@r~�@r�@q�^@q��@qhs@qG�@q%@pĜ@pb@o;d@n��@n5?@m�-@mp�@m?}@mV@l�/@k��@jJ@i��@ihs@iX@ihs@iX@iG�@i7L@i%@hĜ@hQ�@hA�@h �@h �@g�@g��@g|�@gl�@g�@f�y@f�R@f��@fv�@f{@e�-@e�h@e`B@eO�@e�@d�@d��@dj@d�@c�m@cƨ@ct�@co@b�!@bn�@b^5@b�@a�^@a7L@`��@_�@_K�@_+@^�y@^��@^E�@^{@]�@]�T@]O�@\(�@[�m@[�F@[dZ@[o@Z��@Z��@Z~�@Zn�@ZM�@Z=q@Z-@Y��@Y��@Y%@X�u@XA�@W��@V��@V�R@V��@V$�@U��@U�h@U`B@U/@T�/@Tj@SdZ@R�H@R�H@R��@R��@R�!@Rn�@R-@Q�#@Q��@Q��@Q��@Qx�@Q&�@P�`@PbN@Pb@O�;@O+@N��@NV@N5?@N$�@M��@M`B@M/@L��@L��@L��@L�D@Lz�@LZ@L(�@K�
@K�@KC�@K@J�H@J�H@J��@J��@J�\@J�@I�7@IG�@H��@Hr�@HA�@Hb@G�w@G\)@F�@F�+@Fff@F5?@F{@F@E�@E�T@E�-@E/@D��@D�j@D�j@D�j@D�j@D�j@D�j@D�@D��@Dz�@DI�@C��@Cƨ@C33@B��@Bn�@BM�@B=q@BJ@A��@A&�@@Ĝ@@�u@@r�@@A�@@1'@@b@@b@@  @?�;@?�w@?l�@?;d@?+@?
=@>�R@>V@=��@=�@=�@=/@<�/@<�@<z�@<(�@;�
@;�F@;��@;��@;�@;C�@;"�@;@:�H@:��@:=q@9�7@9G�@97L@9%@8�`@8Ĝ@8��@8�@8r�@8�@8r�@8r�@8 �@7��@7
=@6��@6v�@6$�@6@5��@5�-@5�@5p�@5p�@5?}@4��@4�j@4��@3�m@3��@3"�@2�H@2�\@2=q@2�@1�@1x�@1hs@1G�@1G�@1G�@17L@0��@0r�@0A�@0 �@0  @/�@/��@/+@.��@.�R@.�+@.�+@.�+@.ff@.$�@-�@-��@-`B@-/@,�/@,z�@+�
@+dZ@*�H@*��@*�\@*=q@)�@)��@)�^@)G�@)%@(��@(��@(Ĝ@(�@(A�@( �@(b@(  @'��@'�@'��@'|�@'|�@'l�@'�@&�@&��@&V@&5?@%@%p�@%�@$z�@$(�@#ƨ@#�@#o@"�@"�H@"��@"�!@"�!@"�\@"M�@"-@"�@"J@!�@!��@!X@!&�@ ��@ Ĝ@ �@ bN@ 1'@   @��@l�@\)@l�@K�@+@�@�@�@�@�@+@�@�@v�@5?@$�@@@��@p�@I�@C�@33@33@"�@�H@��@�!@�\@~�@n�@^5@�@�^@x�@G�@&�@�@%@��@Ĝ@A�@�@�;@�@�P@|�@l�@K�@�@ff@E�@$�@@@�@�T@�-@p�@`B@?}@V@�/@��@j@j@Z@(�@�
@��@S�@33@o@o@@@@�H@�\@-@��@��@%@�9@�@bN@A�@A�@1'@b@�;@�w@�P@K�@+@
=@��@�y@��@��@@�-@p�@O�@O�@?}@�@V@��@�/@��@��@��@�/@�@�D@�D@�D@�D@�D@�D@�D@�D@z�@9X@�@��@�F@��@t�@dZ@C�@"�@@
��@
^5@
-@
-@
-@	�@	�^@	��@	�7@	�7@	�7@	G�@	7L@	7L@	&�@	%@�9@�@bN@A�@A�@b@�w@l�@\)@+@
=@
=@�R@�+@ff@V@V@{@�@�@�T@@�@?}@V@��@��@�/@��@�@�@��@�D@j@Z@I�@I�@��@��@��@�@dZ@dZA��uA���A���A��uA��uA���A���A���A��uA��hA��uA��uA���A���A��uA���A��uA���A��uA���A���A���A���A���A��uA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA���A���A���A���A���A��uA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��uA��uA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA���A���A��hA��DA�z�A�l�A�I�A�A�x�A�-A�bA��A�E�A�A�ĜA��-A��A��A�5?A�33A���A���A�$�A�bA���A��A���A�r�A�dZA�A�bA���A�v�A���A�ffA��A�XA�&�A���A��mA�ĜA�p�A��A�1A�7LA�O�A��A��A���A���A�S�A��jA��\A�1A��A�A�A��A�x�A�  A�;dA���A��A��A�`BA�C�A�A�ƨA��RA���A�~�A�bNA�M�A�=qA�+A�VA��mA�(�A��#A��A�1A���A��hA��+A�l�A�33A�bA��mA���A��jA���A��A�v�A�VA�A�A�+A�A��`A���A��^A���A�v�A�9XA���A��/A���A��!A���A�x�A�jA�\)A�O�A�M�A�;dA�+A��A�
=A�A���A���A���A��A��A��A��A��;A�ƨA�A��^A��A���A���A��A�=qA�1A���A��TA���A���A���A���A��\A��DA�v�A�O�A�{A�A��A��yA��;A���A���A�XA�9XA� �A�|�A�G�A�=qA�5?A�-A�
=A���A���A�A��9A���A���A���A���A���A��\A��A�x�A�t�A�p�A�n�A�\)A�O�A�(�A��A��^A�~�A��A��A��DA�jA�^5A�VA�O�A�C�A�5?A�bA�  A��A��A��HA�ƨA��\A�S�A��A���A���A��wA��!A���A�jA�O�A�(�A��`A�~�A�/A���A���A�n�A�ZA�=qA�&�A��A��A�JA�%A�A���A��A�ƨA�jA��A��jA��A�dZA�S�A�K�A�;dA�+A� �A�oA�bA�
=A�A���A��A��TA���A���A��^A���A���A��\A�p�A�ffA�9XA�&�A��A�VA�%A���A��/A�x�A��A��
A�ȴA���A���A�z�A�?}A�bA��/A���A��A�G�A�;dA� �A���A�z�A��A~�/A~n�A}�A}��A}hsA}�A|�uA|  A{��A{�-A{�-A{��A{`BA{C�A{�A{oA{oAz��Az�Az�RAz��AzjAzbNAzE�Az5?Az�AzbAzJAy�mAy�
Ay��Ay�^Ay�Ay��Ay��Ay�AydZAy?}Ay�Ax��Ax�RAx��Ax-Aw�Aw�PAwdZAwS�Aw�Av�/Av�uAv$�AuƨAu��Au��Au��Au�hAu�7Au�Aul�Au;dAt��AtI�Ar~�AqO�AqG�Aq�Ap�ApȴAp�ApZApVAp=qAp$�ApbAp1Ap  Ao�TAo��Ao��AoK�Ao�An�An�An�HAn��An��AnffAnM�An�Am�Am�Am|�AmK�Am33Am&�Am�Am%Al��Al�HAl�Al��Al��Al�!Al��Al�\Al�Alv�Aln�AlffAlQ�Al5?Al-Al  Ak��Ak��Ak��Al  Ak��Ak�Ak�TAk�#Ak��Ak�
Ak��Ak�wAk�-Ak��Ak\)Aj��Aj��Ajr�Ai��Ai�TAi��Ai��Ai�FAi�hAi�Aip�Ai`BAiK�Ai7LAi�Ah��Ahv�AhA�Ah{Ah1AhAh  Ag��Ah  Ag�Agx�AgoAf�9Afn�AfM�Af-Af{AfAf  Ae�Ae�Ae�#Ae��Ae��Ae�wAe�Ae�Ae��Ae��Ae�7Ae�Aet�Ae`BAeS�AeO�Ae7LAe�AdĜAd�\AdffAc�Abn�Ab�AaAa�PAa`BAa�AaA`��A`�A`��A`�jA`�RA`��A`bNA`-A_�A_�A_hsA_O�A_/A^�A^��A^�A^�+A^ZA^E�A^5?A]�A]�mA]��A]�wA]�A]��A]��A]�A]7LA]VA]%A\��A\�yA\�HA\��A\z�A\ffA\^5A\^5A\ZA\M�A\I�A\A�A\1'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                              A���A���A���A���A���A���A���A���A���A��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�A���A���A�A�
=A�A���A��uA���A��+A���A���A���A�/A�bNA��uA�1'A���A���A�S�A��-A� �A�bNA�+A��9A��A�9XA��A�-A���A��9A��uA�E�A���A��wA�&�A��A���A��A��A��A�%A~bNA|5?A{?}Az��Az{Ay�FAy�AwƨAvI�Au�7As`BAp�9Ao�mAnȴAm�Al��AlZAk�Ak��Aj1Ai;dAh{Af��Ae�mAe��Ad��Aa�#A`��A_|�A^E�A]�A\�!A\9XA[��A[�AYAXA�AV~�AVjAV=qAU��ATjAT�AS�FARA�AQ33APȴAP �AOG�AN �ALĜAL5?AK�TAJ��AI��AH��AH1AGO�AF��AEƨAC��AB�DAA��A@�A@~�A@$�A?�
A?\)A=��A=`BA="�A;C�A:-A9XA8��A8v�A7��A6��A5x�A4�/A49XA3A3hsA2��A1�;A0�A0-A/\)A.��A.{A-�A,=qA+hsA+�A*��A*^5A)�;A(n�A'�7A'&�A&��A&VA%�TA$��A#ƨA"ȴA"bNA"JA!�^A!hsA!K�A �/A�A;dA�\A1'A�#A&�A��A��A�RA�A5?A�At�Av�A-A��A��A�jAz�A9XA��A|�AhsAK�A�9A �A�A"�A�TA�A$�A�A$�AȴAbNA=qA�#AZAoA
bA	��AA�A�-AS�A�An�A9XAA��A��A�A?}A��A�A ��@��@���@�7L@�=q@��P@�V@�F@�R@�p�@���@�7L@�b@�+@陚@���@�  @��y@�%@�D@�r�@�Z@�b@��`@���@��@��T@�z�@�\)@��y@���@�-@ّh@׶F@ղ-@�r�@�1@ӕ�@��y@�ff@���@���@� �@�^5@��/@��m@�~�@�G�@ȣ�@�j@�33@�%@ÍP@��D@��@�S�@�v�@���@�/@���@���@�r�@�1'@��
@���@��@�J@���@�S�@���@�Z@�S�@��@��@�O�@�/@��9@�bN@�A�@��P@��@��T@�?}@���@���@� �@�$�@��D@�b@��;@��@�+@���@��@�1'@��y@�$�@��^@�hs@��9@�r�@�Q�@�1@���@��@���@��@���@�/@�bN@��w@�\)@�33@��y@���@�J@�x�@��@��9@���@�bN@�dZ@�@��R@�$�@�`B@��`@���@�A�@���@���@�S�@�"�@���@�M�@�{@��T@���@�hs@�/@��/@��D@�A�@�b@��
@�dZ@��@��H@���@�ff@�-@�@���@��7@�7L@���@��/@��u@�1@���@��\@�=q@�{@��@���@��@�X@�&�@�%@��@��@�Q�@��m@��P@�@��+@�E�@��@�J@��@��^@�x�@�X@�7L@���@��@�9X@��m@�ƨ@��@�l�@�33@��@��!@�V@�V@��T@��7@�p�@�X@�?}@�7L@�%@���@��`@��j@�r�@l�@~��@~ȴ@~�+@~E�@}�@|�@|(�@{�
@z��@z^5@y��@y7L@xbN@xb@w�;@w\)@v��@vȴ@vv�@u�T@u�@t�@t��@t�j@t�D@tZ@t1@s��@s"�@r~�@r�@q�^@q��@qhs@qG�@q%@pĜ@pb@o;d@n��@n5?@m�-@mp�@m?}@mV@l�/@k��@jJ@i��@ihs@iX@ihs@iX@iG�@i7L@i%@hĜ@hQ�@hA�@h �@h �@g�@g��@g|�@gl�@g�@f�y@f�R@f��@fv�@f{@e�-@e�h@e`B@eO�@e�@d�@d��@dj@d�@c�m@cƨ@ct�@co@b�!@bn�@b^5@b�@a�^@a7L@`��@_�@_K�@_+@^�y@^��@^E�@^{@]�@]�T@]O�@\(�@[�m@[�F@[dZ@[o@Z��@Z��@Z~�@Zn�@ZM�@Z=q@Z-@Y��@Y��@Y%@X�u@XA�@W��@V��@V�R@V��@V$�@U��@U�h@U`B@U/@T�/@Tj@SdZ@R�H@R�H@R��@R��@R�!@Rn�@R-@Q�#@Q��@Q��@Q��@Qx�@Q&�@P�`@PbN@Pb@O�;@O+@N��@NV@N5?@N$�@M��@M`B@M/@L��@L��@L��@L�D@Lz�@LZ@L(�@K�
@K�@KC�@K@J�H@J�H@J��@J��@J�\@J�@I�7@IG�@H��@Hr�@HA�@Hb@G�w@G\)@F�@F�+@Fff@F5?@F{@F@E�@E�T@E�-@E/@D��@D�j@D�j@D�j@D�j@D�j@D�j@D�@D��@Dz�@DI�@C��@Cƨ@C33@B��@Bn�@BM�@B=q@BJ@A��@A&�@@Ĝ@@�u@@r�@@A�@@1'@@b@@b@@  @?�;@?�w@?l�@?;d@?+@?
=@>�R@>V@=��@=�@=�@=/@<�/@<�@<z�@<(�@;�
@;�F@;��@;��@;�@;C�@;"�@;@:�H@:��@:=q@9�7@9G�@97L@9%@8�`@8Ĝ@8��@8�@8r�@8�@8r�@8r�@8 �@7��@7
=@6��@6v�@6$�@6@5��@5�-@5�@5p�@5p�@5?}@4��@4�j@4��@3�m@3��@3"�@2�H@2�\@2=q@2�@1�@1x�@1hs@1G�@1G�@1G�@17L@0��@0r�@0A�@0 �@0  @/�@/��@/+@.��@.�R@.�+@.�+@.�+@.ff@.$�@-�@-��@-`B@-/@,�/@,z�@+�
@+dZ@*�H@*��@*�\@*=q@)�@)��@)�^@)G�@)%@(��@(��@(Ĝ@(�@(A�@( �@(b@(  @'��@'�@'��@'|�@'|�@'l�@'�@&�@&��@&V@&5?@%@%p�@%�@$z�@$(�@#ƨ@#�@#o@"�@"�H@"��@"�!@"�!@"�\@"M�@"-@"�@"J@!�@!��@!X@!&�@ ��@ Ĝ@ �@ bN@ 1'@   @��@l�@\)@l�@K�@+@�@�@�@�@�@+@�@�@v�@5?@$�@@@��@p�@I�@C�@33@33@"�@�H@��@�!@�\@~�@n�@^5@�@�^@x�@G�@&�@�@%@��@Ĝ@A�@�@�;@�@�P@|�@l�@K�@�@ff@E�@$�@@@�@�T@�-@p�@`B@?}@V@�/@��@j@j@Z@(�@�
@��@S�@33@o@o@@@@�H@�\@-@��@��@%@�9@�@bN@A�@A�@1'@b@�;@�w@�P@K�@+@
=@��@�y@��@��@@�-@p�@O�@O�@?}@�@V@��@�/@��@��@��@�/@�@�D@�D@�D@�D@�D@�D@�D@�D@z�@9X@�@��@�F@��@t�@dZ@C�@"�@@
��@
^5@
-@
-@
-@	�@	�^@	��@	�7@	�7@	�7@	G�@	7L@	7L@	&�@	%@�9@�@bN@A�@A�@b@�w@l�@\)@+@
=@
=@�R@�+@ff@V@V@{@�@�@�T@@�@?}@V@��@��@�/@��@�@�@��@�D@j@Z@I�@I�@��@��@��@�@dZG�O�A��uA���A���A��uA��uA���A���A���A��uA��hA��uA��uA���A���A��uA���A��uA���A��uA���A���A���A���A���A��uA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA���A���A���A���A���A��uA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��uA��uA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA���A���A��hA��DA�z�A�l�A�I�A�A�x�A�-A�bA��A�E�A�A�ĜA��-A��A��A�5?A�33A���A���A�$�A�bA���A��A���A�r�A�dZA�A�bA���A�v�A���A�ffA��A�XA�&�A���A��mA�ĜA�p�A��A�1A�7LA�O�A��A��A���A���A�S�A��jA��\A�1A��A�A�A��A�x�A�  A�;dA���A��A��A�`BA�C�A�A�ƨA��RA���A�~�A�bNA�M�A�=qA�+A�VA��mA�(�A��#A��A�1A���A��hA��+A�l�A�33A�bA��mA���A��jA���A��A�v�A�VA�A�A�+A�A��`A���A��^A���A�v�A�9XA���A��/A���A��!A���A�x�A�jA�\)A�O�A�M�A�;dA�+A��A�
=A�A���A���A���A��A��A��A��A��;A�ƨA�A��^A��A���A���A��A�=qA�1A���A��TA���A���A���A���A��\A��DA�v�A�O�A�{A�A��A��yA��;A���A���A�XA�9XA� �A�|�A�G�A�=qA�5?A�-A�
=A���A���A�A��9A���A���A���A���A���A��\A��A�x�A�t�A�p�A�n�A�\)A�O�A�(�A��A��^A�~�A��A��A��DA�jA�^5A�VA�O�A�C�A�5?A�bA�  A��A��A��HA�ƨA��\A�S�A��A���A���A��wA��!A���A�jA�O�A�(�A��`A�~�A�/A���A���A�n�A�ZA�=qA�&�A��A��A�JA�%A�A���A��A�ƨA�jA��A��jA��A�dZA�S�A�K�A�;dA�+A� �A�oA�bA�
=A�A���A��A��TA���A���A��^A���A���A��\A�p�A�ffA�9XA�&�A��A�VA�%A���A��/A�x�A��A��
A�ȴA���A���A�z�A�?}A�bA��/A���A��A�G�A�;dA� �A���A�z�A��A~�/A~n�A}�A}��A}hsA}�A|�uA|  A{��A{�-A{�-A{��A{`BA{C�A{�A{oA{oAz��Az�Az�RAz��AzjAzbNAzE�Az5?Az�AzbAzJAy�mAy�
Ay��Ay�^Ay�Ay��Ay��Ay�AydZAy?}Ay�Ax��Ax�RAx��Ax-Aw�Aw�PAwdZAwS�Aw�Av�/Av�uAv$�AuƨAu��Au��Au��Au�hAu�7Au�Aul�Au;dAt��AtI�Ar~�AqO�AqG�Aq�Ap�ApȴAp�ApZApVAp=qAp$�ApbAp1Ap  Ao�TAo��Ao��AoK�Ao�An�An�An�HAn��An��AnffAnM�An�Am�Am�Am|�AmK�Am33Am&�Am�Am%Al��Al�HAl�Al��Al��Al�!Al��Al�\Al�Alv�Aln�AlffAlQ�Al5?Al-Al  Ak��Ak��Ak��Al  Ak��Ak�Ak�TAk�#Ak��Ak�
Ak��Ak�wAk�-Ak��Ak\)Aj��Aj��Ajr�Ai��Ai�TAi��Ai��Ai�FAi�hAi�Aip�Ai`BAiK�Ai7LAi�Ah��Ahv�AhA�Ah{Ah1AhAh  Ag��Ah  Ag�Agx�AgoAf�9Afn�AfM�Af-Af{AfAf  Ae�Ae�Ae�#Ae��Ae��Ae�wAe�Ae�Ae��Ae��Ae�7Ae�Aet�Ae`BAeS�AeO�Ae7LAe�AdĜAd�\AdffAc�Abn�Ab�AaAa�PAa`BAa�AaA`��A`�A`��A`�jA`�RA`��A`bNA`-A_�A_�A_hsA_O�A_/A^�A^��A^�A^�+A^ZA^E�A^5?A]�A]�mA]��A]�wA]�A]��A]��A]�A]7LA]VA]%A\��A\�yA\�HA\��A\z�A\ffA\^5A\^5A\ZA\M�A\I�A\A�A\1'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                              ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B�MB��B�MB��B��B��B��B��B��B��B��B��B��B�MB�MB�MB�MB�B��B�{B��B��B�B{�Bu%Bd�B/B%B��BҽB��B�kBr|Ba�Bd�BcTB_;B`vB`�BZBV�BT,BS&BR�BK)BF�BEmB5?B/OB*eB(�BB	7B�B
�GB
��B
�dB
ÖB
�RB
��B
�zB
�bB
�IB
��B
��B
�=B
_�B
9�B
,=B
-�B
($B
#B
�B
eB
�B
�B
�B	��B	��B	ںB	�HB	��B	�KB	ĜB	�3B	�UB	��B	��B	��B	�B	�B	�XB	��B	�oB	�VB	��B	��B	�AB	�B	|B	|�B	v�B	sB	h�B	g�B	jB	k�B	dZB	`�B	`�B	\]B	X�B	V�B	U�B	R�B	O�B	J#B	HKB	F�B	E9B	A�B	;dB	9XB	4�B	0�B	.}B	'�B	~B	=B	�B	�B	4B	(B	"B	_B	�B	B��B��B�;B�B�B�yB�,B�5B��B�B�,B҉B�B�<B�tB�9B�OB��B�B��B��B�UB�CB��B�*B��B�B�OB��B��B��B�SB��B��B�xB��B��B��B��B��B�B��B}"Bz�BxlBxBw�Bt�Bs�Bs�BsBrBpBqvBm]BkBjKBhsBf�Be�Be`BdZBcTBbBa|B`�B_;B[�B[�BYKBV9BT,BO�BO�BK�BHBGzBGBEBA�B>�B<jB<�B8�B7�B8RB6�B5�B4nB33B3hB1'B/B,�B,�B)�B($B%�B&LB%B!bB \B�BIB�BB�BeB+B�BSBBSBSB{B�B�B�B�B�B�B�BFB:B�B�B4B\B@B�B\B�B\B�B�B"B�B�B�B"B�B�B�B\B"BoB:BB�B�B�BeB�BkB�B�B	B=BqB=BCB~BOB!�B$�B%FB'B*0B*eB*�B+B,qB,�B-B-�B/�B1�B2�B2�B2aB2�B9�B<�B>BB>�B?BA BB�BEmBK^BQ�BS[BU2BVmBYBY�BZB[#B]/B`B`vBc�Bc�Be,Bh
Bi�BjBjBk�BlWBo�BrBtBv+Bv�BxlB~(B�B�B��B�7B��B�PB��B��B��B��B�MB��B��B�IB��B��B��B�B�B�RB�*B�eB��B�B��B�aB�B�tB�LB�RB��B��B�qB��B�B��B��B�B��B�0BΥB�}BуB��B�gB�
B�BیB��B�B�B�B��B�B�]B�/B�cB�5B�B�MB��B�B��B�lB��B��B��B��B��B��B	�B	{B	�B	�B	~B	.B	�B	�B	oB	�B	FB	{B	�B	�B	YB	�B	�B	 �B	"4B	#�B	(XB	,�B	.IB	/B	2�B	4�B	6�B	9�B	<jB	<�B	=�B	@OB	AUB	B'B	E9B	H�B	JXB	J�B	K)B	K^B	LdB	M6B	N�B	P�B	R�B	VB	W
B	X�B	YKB	Y�B	Z�B	[WB	[�B	^�B	bNB	e�B	g8B	jKB	kQB	l�B	n/B	o B	w�B	{�B	}VB	~�B	cB	.B	cB	�B	� B	��B	�;B	�B	�GB	��B	��B	��B	��B	��B	��B	�_B	��B	�B	��B	�=B	�~B	�VB	��B	��B	��B	�4B	�:B	�uB	�B	�B	��B	�B	�YB	��B	��B	��B	�B	�B	��B	��B	�4B	��B	��B	�_B	�0B	�kB	��B	�CB	��B	��B	�!B	�nB	�B	�B	��B	�XB	�^B	��B	�dB	��B	�B	�6B	�6B	��B	��B	��B	��B	��B	ŢB	��B	ȴB	��B	��B	��B	̘B	�6B	��B	��B	�}B	ԕB	֡B	�mB	�
B	�?B	�?B	�yB	ٴB	چB	�WB	�#B	یB	��B	�/B	ݘB	ߤB	��B	�B	�TB	��B	��B	��B	�2B	�
B	�B	�DB	�B	�B	��B	�B	�B	�QB	��B	�)B	��B	�cB	�B	�iB	�B	�B	�;B	�oB	�B	�MB	�B	��B	�+B	��B	�2B	�B	�	B	�xB	�B	�JB	��B	�PB	�PB	�PB	��B	�VB	�]B	��B	�cB	��B	��B	��B	��B	�cB	��B	��B	��B
 �B
B
�B
B
�B
�B
�B
�B
SB
%B
�B
fB
�B
	7B
	7B
	�B
	�B
	�B

	B

	B

	B
xB
�B
B
�B
�B
�B
�B
(B
�B
�B
.B
�B
hB
�B
�B
�B
�B
B
@B
�B
�B
�B
FB
B
�B
$B
$B
�B
�B
�B
�B
eB
eB
�B
eB
�B
1B
�B
	B
�B
�B
�B
�B
�B
VB
!B
 \B
�B
VB
 �B
 \B
!-B
 �B
"�B
#nB
$tB
$tB
%zB
%�B
%�B
&B
'�B
'B
'RB
'RB
'RB
'�B
($B
(�B
)*B
)�B
)�B
)�B
*0B
*�B
+B
+�B
,qB
+�B
+�B
+�B
,qB
-B
-�B
-�B
.}B
.�B
.�B
0UB
1[B
2aB
1�B
2-B
2�B
3�B
3�B
4B
5tB
5?B
4�B
5B
5tB
6FB
6�B
6�B
6FB
6zB
7B
7LB
7�B
7�B
7LB
7B
7�B
8B
8RB
9XB
9$B
:^B
:�B
;dB
<6B
=B
=�B
=qB
?B
>�B
>�B
?B
?}B
?}B
?�B
?�B
?�B
?�B
@�B
@OB
@�B
A�B
A�B
A�B
B�B
B�B
CaB
C�B
D3B
D�B
E�B
FB
EmB
E�B
F�B
FtB
F�B
F?B
FtB
FtB
F?B
F�B
F�B
G�B
HB
G�B
HB
H�B
H�B
H�B
J�B
L0B
L0B
L0B
LdB
L�B
M6B
MjB
MjB
M�B
M�B
M�B
N<B
N<B
NpB
N<B
OB
OB
OB
OB
OvB
OvB
O�B
OvB
O�B
O�B
O�B
O�B
PB
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R�B
RTB
R�B
S[B
S&B
T,B
S�B
T,B
T,B
T�B
U2B
U�B
U�B
VmB
V9B
V�B
V9B
V�B
VmB
V�B
W?B
XyB
XB
X�B
YB
ZQB
Z�B
ZQB
[#B
Z�B
Z�B
[#B
[�B
[�B
\]B
]/B
]dB
]dB
]�B
^jB
^jB
]�B
^jB
_;B
_;B
_�B
_;B
_�B
`BB
`B
`BB
`�B
`vB
`�B
`�B
`�B
`�B
aHB
aHB
aB
aB
`�B
aHB
aB
aB
aHB
a�B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
b�B
c B
c�B
dZB
e,B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e`B
f�B
f2B
e�B
f2B
f�B
f�B
g�B
g�B
g�B
g�B
h
B
h�B
h�B
iB
iyB
i�B
i�B
jB
jB
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l"B
lWB
l�B
l"B
l�B
l�B
m)B
m)B
m]B
m�B
m]B
m]B
m]B
l�B
m�B
n/B
m�B
m�B
ncB
n/B��B�B��B�SB��B��B��B��B��B�SB��B��B�{B��B��B�B�SB��B��B�uB��B��B��B��B��B��B��B�{B�B�MB��B�B�B�MB��B��B�MB�B�MB�B�B��B��B��B��B�{B��B�MB��B��B�SB��B�B�{B�{B��B�B��B�B��B��B��B�B�{B�GB�MB�MB��B�SB�B�SB�B�MB��B��B��B��B�B��B�SB�SB�B��B��B��B�{B��B��B��B�MB�MB�B�SB��B��B��B�B��B�{B�{B�MB��B��B�B�B�B��B�MB��B�{B�{B��B�B��B�SB�SB��B��B��B�{B��B��B�MB��B��B��B�B�B�MB��B��B�{B�GB�B��B�MB�MB��B��B�B�B��B��B�MB��B�B��B�B�GB�GB�{B��B��B��B��B��B�MB��B��B��B��B��B��B�B�{B��B��B�B��B�B�B�AB�AB�uB�uB�B��B��B�B��B�uB�AB��B��B� B�B.BcB.B~�B~�B}�B~�B~(B}�B~(BzDB|BzBwfBv�Bu�BsMBzxBr�B�Bh>Be�BcTB��BYKB��BN�BN�BJ�Bv`BgB6�B6�BIBJB
�B�B��B\B�cB+BuB��BB�B+B��B��B�KB��B�6B�BҽBٴB�EB��BٴB�$B��B�IB��B��B�tB��B�IB��B�LB�XB�mB�@Bm)B��ByrBqvBc�Bd�BoiBb�B`�Bg�Bc B^�B_B_B]�B_Bc�Bv+Be�BiBjB`vB^�B`BBa�B_�Ba�B`vB\�B_B`�B\)B`BBaHBa�B_;Ba�B_�B]dB`vB]�B_�Bg8B_�B^�B]�B[�BZ�BaBU�BVmBW?BU�BV�BV�BW
BT�BU2BU2BT�BTaBS�BR BR�BQ�BUgBS�BQ�BQNBS�BRTBR BW?BXyBRTBNBK�BK^BJ�BQ�BL0BI�BG�BK)BOBBJ�BB�BD�BAUB?}BB�BE9BP}B=<B>�B[WB8RB3�B1�B2�B6FB4�B/�B1'B.}B.IB,�B+�B+�B+B+�B+�B*eB(�B'�B(XB'RB'B+kB'B%�B#:B!bB�B �BVB�B
=B	�B
	B	�B�B�BB�B�BMB�B �B
�]B
�B
�fB
�;B
�B
�oB
�mB
�>B
�B
�oB
��B
�B
��B
�gB
�<B
�6B
�B
�^B
�RB
�mB
�B
��B
� B
� B
�-B
��B
�B
�UB
��B
��B
�hB
��B
�0B
��B
�qB
��B
�B
�B
�B
��B
�B
��B
��B
��B
�FB
�nB
��B
�4B
�!B
��B
�'B
�4B
�B
�OB
�kB
��B
��B
�\B
�eB
�RB
��B
��B
��B
�_B
�kB
�B
�YB
��B
��B
�~B
�"B
�MB
��B
��B
��B
�B
tTB
c�B
T�B
_pB
B�B
LdB
AUB
B�B
.B
/�B
-B
.B
*�B
,=B
+B
+6B
,B
.�B
1[B
-�B
/OB
/�B
*�B
*�B
'RB
)_B
(�B
&LB
'�B
'�B
$�B
#nB
"4B
 �B
!-B
 �B
!�B
�B
�B
�B
$�B
IB
'�B
�B
�B
�B
�B
�B
_B
eB
�B
�B
�B
B
	7B
�B
+B
%B
�B
AB
DB	�VB
,�B	�B	�
B	��B	��B	�B	�B	��B	�B	�B	�|B	� B	�;B	��B	�B	ߤB	ߤB	��B	��B	�B	�B	�QB	��B	چB	�B	��B	רB	�EB	�vB	�TB	�BB	��B	˒B	�#B	˒B	��B	ɆB	�RB	�B	�XB	�#B	�EB	�B	��B	ȀB	�EB	�B	�zB	�KB	��B	�EB	��B	ĜB	�gB	�-B	�3B	�B	�gB	ƨB	��B	��B	��B	�gB	�-B	ÖB	��B	��B	�OB	�aB	ȀB	�}B	�wB	�<B	��B	� B	�wB	��B	�wB	��B	��B	��B	ŢB	�B	��B	�B	�FB	��B	�B	��B	�nB	��B	�B	��B	��B	��B	�aB	��B	��B	��B	�B	�OB	��B	��B	��B	�wB	��B	��B	��B	��B	�kB	��B	�B	�_B	�kB	��B	��B	�XB	��B	��B	��B	��B	��B	�zB	��B	�xB	��B	�eB	�B	��B	�uB	��B	�B	��B	�.B	�@B	��B	�oB	��B	��B	�(B	�B	��B	�B	�rB	�xB	�7B	��B	��B	��B	��B	��B	��B	��B	�_B	��B	��B	�%B	�bB	��B	�AB	��B	�B	�B	��B	�iB	�B	��B	cB	.B	�;B	.B	cB	� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                              B��B��B��B��B��B��B��B�bB��B�/B�B��B��B�wB��B��B��B�vB��B��B�MB�CB�LB�BB�B��B�oB��B��B�lB��B�B�IB>_B�B��B�BԜB̃Bz�Bh�Bp7Bh�Bc`BemBepB\�BX!BUwBV1BV�BN�BK�BL�B8DB0�B-B0sB �BB�B
��B
�B
�+B
�B
��B
�oB
��B
��B
��B
��B
�nB
��B
gB
=LB
.\B
/�B
)|B
%7B
#HB
$B
�B
+B
�B	��B	�B	��B	ҨB	ˁB	ɢB	�B	�IB	�5B	��B	�WB	��B	�oB	��B	��B	��B	��B	�RB	�KB	�iB	��B	�hB	~�B	�SB	|B	x_B	i�B	h�B	l7B	o�B	e�B	b�B	e�B	_�B	Z�B	Y8B	X�B	V�B	T@B	L>B	I�B	JPB	I6B	D�B	=�B	;�B	7OB	4aB	5PB	+�B	 �B	bB	�B	%B	fB	B	�B		B	,B	-B�B��B�B�nB�&B�)B�B�rB�"BٶBՙB�iB��B�tB�JB��B�aB��B�OB��B�pB�xB�xB�xB�;B��B�8B��B�B�qB�xB��B��B�-B�B�B�-B� B�!B��B�.B�GB�B|UBy�BzuBx�BuBt.Bt�Bt3Br�Br�Bt�Bn�Bm%Bl�BizBg�BgBf�Be�Bc�Bb�Bc�Bb�B`�B^DB_�B\OBY�BW�BS�BTYBMOBH�BIXBLfBI~BEkBA B@�B>�B:}B9UB9�B7�B6�B5�B6/B7B3�B1DB0B1CB,�B*�B(�B+�B*B%�B"�B�B�B�B �B�BB�B�B�B:B}B2B�BB�BGB�B"B�BNB&BB�B�B|B�B�B�B2B�B�B�BB�BB0B�BB�B�B�B�B�B�BZBCBMB�BGB�B�B*BYBBB�B�BB�BB�B!�B%CB'B'pB)�B+B*�B+6B+�B-B-B.xB/"B1�B2�B3pB3dB3�B6sB<�B=�B>�B?-B@/BB\BDBH�BM�BR�BTABU�BW�BZ
BZBZ�B\B^�B`�Ba�BdsBd�Bf�BiRBj|Bj�Bk!BlwBm|Bp�Br�Bt�BvhBwPBz`B~�B�vB�BB�wB�5B�wB�B�:B��B�eB�.B�lB�hB�&B��B��B��B�B��B��B��B��B��B��B��B�B��B��B��B��B��B�.B�rB��B�6B��B�B�~B�aB˙B̘B� B��B�!B�YB��B�cB��B��B�wB��B��B�B��B�BB��B�aB�B�B�/B�B�@B�B�8B�XB��B�
B��B�HB�B	 ^B	fB	*B	�B	�B	-B	iB		B	�B	�B	B	sB	�B	B	1B	�B	�B	 2B	!B	"�B	$uB	))B	-)B	.�B	0B	3yB	5B	7�B	:�B	<�B	<�B	>.B	@�B	A�B	B�B	E�B	I;B	J�B	J�B	KHB	K�B	L�B	M�B	OJB	Q=B	S5B	VjB	WsB	X�B	Y�B	ZB	Z�B	[�B	\�B	_�B	b�B	fgB	g�B	j�B	k�B	m/B	n�B	ptB	yB	|&B	}�B	~�B	_B	IB	}B	�B	�=B	��B	��B	�,B	�pB	��B	��B	��B	��B	��B	�IB	��B	�8B	�-B	��B	��B	��B	�B	�+B	�B	��B	�tB	��B	��B	�hB	�QB	��B	�xB	��B	�cB	��B	��B	�\B	��B	�B	�B	�RB	��B	�!B	��B	�{B	��B	�B	�lB	�B	��B	�1B	��B	�JB	�oB	�B	��B	��B	��B	�}B	��B	�B	�QB	�sB	�B	�pB	�[B	��B	�qB	�9B	�'B	��B	�_B	�B	�B	��B	�oB	�.B	�]B	�B	�B	֧B	ֆB	�!B	�ZB	׋B	��B	�B	ںB	�^B	�2B	��B	�B	�xB	�B	��B	�B	��B	��B	�B	�$B	�B	�B	�OB	��B	�{B	��B	�JB	��B	��B	�DB	�B	�FB	�B	�AB	�B	��B	�tB	��B	�,B	�^B	��B	�7B	�B	�B	�BB	�bB	�B	��B	�nB	��B	��B	�=B	��B	�B	�fB	�gB	�jB	��B	��B	��B	��B	�lB	��B	��B	��B	��B	�zB	��B	��B
 B
 �B
GB
9B
�B
8B
�B
B
#B
�B
�B
�B
�B
�B
	jB
	PB
	�B
	�B
	�B

/B

2B

aB
�B
�B
<B
B
B
�B
B
3B
�B
JB
fB
B
�B
!B
�B
�B
�B
'B
�B
�B
�B
B
sB
�B
�B
fB
CB
(B
B
�B
�B
�B
zB
�B
{B
�B
�B
YB
�B
B
B
B
�B
�B
|B
WB
 kB
�B
�B
 �B
 �B
!\B
!�B
"�B
#�B
$�B
$�B
%�B
&B
&B
&�B
'�B
'CB
'XB
'YB
'pB
'�B
(�B
)*B
)QB
)�B
)�B
* B
*�B
+B
+FB
+�B
,tB
+�B
+�B
,B
,�B
-bB
."B
-�B
.�B
/SB
/TB
0�B
1�B
2zB
2B
2�B
3B
3�B
3�B
4{B
5�B
5VB
4�B
5&B
5�B
6�B
6�B
6�B
6`B
6�B
7:B
7dB
7�B
7�B
7hB
7mB
8-B
8dB
8�B
9�B
9�B
:�B
;VB
<B
<�B
=oB
>B
=�B
?3B
>�B
>�B
?7B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
A/B
A�B
A�B
A�B
B�B
B�B
C�B
DB
DlB
E.B
E�B
FB
E�B
E�B
F�B
FzB
F�B
FEB
FwB
FlB
FZB
F�B
G?B
G�B
H-B
HB
HYB
H�B
H�B
I�B
K�B
LEB
L7B
LIB
L�B
L�B
MMB
M�B
MB
M�B
M�B
NB
N�B
N�B
N�B
NbB
O"B
O#B
O'B
OKB
O�B
O�B
O�B
O�B
PB
O�B
O�B
PB
P�B
QVB
Q�B
Q�B
Q�B
Q�B
R B
Q�B
R!B
R`B
R�B
R}B
R�B
S�B
SlB
TYB
T B
TDB
TgB
T�B
UxB
VB
U�B
V�B
VAB
V�B
V@B
V�B
V�B
W,B
W�B
X�B
XB
Y=B
Y�B
Z�B
[B
ZwB
[%B
Z�B
[B
[ZB
[�B
[�B
\�B
]RB
]�B
]|B
]�B
^�B
^�B
^\B
^�B
_{B
__B
_�B
_SB
_�B
`UB
`%B
`fB
`�B
`|B
`�B
`�B
`�B
aB
aLB
aKB
aB
aB
`�B
aKB
aB
a,B
a�B
a�B
a�B
b*B
b�B
b�B
b�B
b�B
b�B
cB
ciB
dPB
d�B
e,B
d�B
eB
e-B
e�B
e�B
fB
e�B
e�B
f�B
f9B
fB
f^B
f�B
gB
g�B
g�B
g�B
g�B
haB
h�B
h�B
iDB
i�B
i�B
j3B
j�B
j�B
j�B
j�B
j�B
kB
k�B
k�B
k�B
l B
lB
lSB
lkB
l�B
lIB
mB
l�B
m-B
m=B
msB
m�B
mrB
mqB
mgB
mB
m�B
nBB
m�B
m�B
ndG�O�B��B�B��B�SB��B��B��B��B��B�SB��B��B�{B��B��B�B�SB��B��B�uB��B��B��B��B��B��B��B�{B�B�MB��B�B�B�MB��B��B�MB�B�MB�B�B��B��B��B��B�{B��B�MB��B��B�SB��B�B�{B�{B��B�B��B�B��B��B��B�B�{B�GB�MB�MB��B�SB�B�SB�B�MB��B��B��B��B�B��B�SB�SB�B��B��B��B�{B��B��B��B�MB�MB�B�SB��B��B��B�B��B�{B�{B�MB��B��B�B�B�B��B�MB��B�{B�{B��B�B��B�SB�SB��B��B��B�{B��B��B�MB��B��B��B�B�B�MB��B��B�{B�GB�B��B�MB�MB��B��B�B�B��B��B�MB��B�B��B�B�GB�GB�{B��B��B��B��B��B�MB��B��B��B��B��B��B�B�{B��B��B�B��B�B�B�AB�AB�uB�uB�B��B��B�B��B�uB�AB��B��B� B�B.BcB.B~�B~�B}�B~�B~(B}�B~(BzDB|BzBwfBv�Bu�BsMBzxBr�B�Bh>Be�BcTB��BYKB��BN�BN�BJ�Bv`BgB6�B6�BIBJB
�B�B��B\B�cB+BuB��BB�B+B��B��B�KB��B�6B�BҽBٴB�EB��BٴB�$B��B�IB��B��B�tB��B�IB��B�LB�XB�mB�@Bm)B��ByrBqvBc�Bd�BoiBb�B`�Bg�Bc B^�B_B_B]�B_Bc�Bv+Be�BiBjB`vB^�B`BBa�B_�Ba�B`vB\�B_B`�B\)B`BBaHBa�B_;Ba�B_�B]dB`vB]�B_�Bg8B_�B^�B]�B[�BZ�BaBU�BVmBW?BU�BV�BV�BW
BT�BU2BU2BT�BTaBS�BR BR�BQ�BUgBS�BQ�BQNBS�BRTBR BW?BXyBRTBNBK�BK^BJ�BQ�BL0BI�BG�BK)BOBBJ�BB�BD�BAUB?}BB�BE9BP}B=<B>�B[WB8RB3�B1�B2�B6FB4�B/�B1'B.}B.IB,�B+�B+�B+B+�B+�B*eB(�B'�B(XB'RB'B+kB'B%�B#:B!bB�B �BVB�B
=B	�B
	B	�B�B�BB�B�BMB�B �B
�]B
�B
�fB
�;B
�B
�oB
�mB
�>B
�B
�oB
��B
�B
��B
�gB
�<B
�6B
�B
�^B
�RB
�mB
�B
��B
� B
� B
�-B
��B
�B
�UB
��B
��B
�hB
��B
�0B
��B
�qB
��B
�B
�B
�B
��B
�B
��B
��B
��B
�FB
�nB
��B
�4B
�!B
��B
�'B
�4B
�B
�OB
�kB
��B
��B
�\B
�eB
�RB
��B
��B
��B
�_B
�kB
�B
�YB
��B
��B
�~B
�"B
�MB
��B
��B
��B
�B
tTB
c�B
T�B
_pB
B�B
LdB
AUB
B�B
.B
/�B
-B
.B
*�B
,=B
+B
+6B
,B
.�B
1[B
-�B
/OB
/�B
*�B
*�B
'RB
)_B
(�B
&LB
'�B
'�B
$�B
#nB
"4B
 �B
!-B
 �B
!�B
�B
�B
�B
$�B
IB
'�B
�B
�B
�B
�B
�B
_B
eB
�B
�B
�B
B
	7B
�B
+B
%B
�B
AB
DB	�VB
,�B	�B	�
B	��B	��B	�B	�B	��B	�B	�B	�|B	� B	�;B	��B	�B	ߤB	ߤB	��B	��B	�B	�B	�QB	��B	چB	�B	��B	רB	�EB	�vB	�TB	�BB	��B	˒B	�#B	˒B	��B	ɆB	�RB	�B	�XB	�#B	�EB	�B	��B	ȀB	�EB	�B	�zB	�KB	��B	�EB	��B	ĜB	�gB	�-B	�3B	�B	�gB	ƨB	��B	��B	��B	�gB	�-B	ÖB	��B	��B	�OB	�aB	ȀB	�}B	�wB	�<B	��B	� B	�wB	��B	�wB	��B	��B	��B	ŢB	�B	��B	�B	�FB	��B	�B	��B	�nB	��B	�B	��B	��B	��B	�aB	��B	��B	��B	�B	�OB	��B	��B	��B	�wB	��B	��B	��B	��B	�kB	��B	�B	�_B	�kB	��B	��B	�XB	��B	��B	��B	��B	��B	�zB	��B	�xB	��B	�eB	�B	��B	�uB	��B	�B	��B	�.B	�@B	��B	�oB	��B	��B	�(B	�B	��B	�B	�rB	�xB	�7B	��B	��B	��B	��B	��B	��B	��B	�_B	��B	��B	�%B	�bB	��B	�AB	��B	�B	�B	��B	�iB	�B	��B	cB	.B	�;B	.B	cB	� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                              <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���=��<u�#<��b<�}<���=k�=�?<#�
<#�
<8�5<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<7�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2019112021494720191120214947IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019113020002720191130200027QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019113020002720191130200027QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020061910341820200619103418IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617003920210326170039IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617003920210326170039IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617003920210326170039IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                
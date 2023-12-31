CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-10-16T01:17:56Z creation; 2022-02-04T23:30:04Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  c�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ϰ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � (   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � ;�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � Z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � bX   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20211016011756  20220204223518  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_185                 6810_008521_185                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ٛ>�^@ٛ>�^11  @ٛ>Ov_�@ٛ>Ov_�@0iK����@0iK�����dr�!B�dr�!B11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�@E�@�  @�G�@��R@�  AG�A  A�RA,(�A@  A_\)A�Q�A�Q�A�  A�  A���AУ�A�  A�\)A��B(�B  B�
B�B'�
B0  B7�
B@  BH(�BO�
BW�B`  Bh(�Bp  Bw�
B�  B�{B�  B��
B�  B�{B��B��B�{B��B�{B�{B�{B�{B�{B�(�B�  B��B�  B�(�B�(�B�{B�  B��B�  B�{B��B�  B�  B�{B�{B�{C   C  C  C��C��C
  C  C
=C  C��C��C  C
=C�C��C
=C {C"{C$  C&  C({C*
=C,  C.
=C/��C1�C4  C6
=C8
=C:  C<
=C>
=C@  CB  CC��CE�CG��CJ  CK��CM��CO��CR  CT
=CV
=CX  CZ  C\  C^  C_��Cb  Cd
=Ce��Ch  Cj
=Cl
=Cn
=Cp
=Cq��Cs�Cu�Cw�Cy��C{��C}��C�
=C�  C���C���C���C�  C�  C�  C�  C�  C���C�  C���C�  C���C���C�  C���C�  C�  C�C�  C�  C�  C���C�C�  C�C�
=C�C���C���C�C�  C�\C�C���C���C�C���C���C���C���C�  C�
=C�C�  C�  C���C�  C�  C�C�C�C�C�  C���C���C�  C�  C���C�C���C�  C�  C���C�  C�  C�  C���C���C���C�C�  C�  C�C�  C�  C�  C�C�  C�  C�
=C�\C�
=C���C�  C�C�C�  C�  C�  C�C�  C�  C�  C���C�  C���C�  C�C�  C�C�
=C�C���C�  C�  C�  C���C�  C���C�C���C�  C�C�C�C�C�C�C�  C�  C�C�
=C�C���C�  D �D � D  D� D  D� D�D� D�qD� D�D� D��Dz�D�qD}qD  D� D	�D	�D
�D
��D  Dz�D��D}qD  D��D�D��D  Dz�D�qD� D  D}qD�qD� D�D��D�D}qD  D� D�qD� D�qD}qD  D��D�D� D�D�D  D}qD�qD��DD��DD�DD� D��D � D!  D!}qD!�qD"}qD"�qD#� D#��D$}qD%D%��D%��D&}qD'D'��D'��D(}qD)D)��D)�qD*z�D+�D+��D+�qD,}qD,��D-z�D-��D.� D/  D/}qD0  D0}qD1�D1�D1�qD2}qD3  D3}qD4  D4� D4�qD5� D6�D6��D7  D7��D8D8��D9�D9� D9��D:� D:�qD;z�D<�D<��D<�qD=}qD>  D>}qD>��D?� D@�D@��DA�DA}qDA�qDB� DB��DC}qDD�DD�DE�DE��DF  DF}qDG  DG��DH�DH��DI�DI�DJ  DJ� DK�DK��DL  DL}qDL�qDM��DNDN��DO  DOz�DO�qDP��DQ  DQ� DR�DR� DS�DS�DT�DT� DU  DU}qDV  DV}qDV�qDW}qDW�qDX��DY  DY}qDY�qDZ� D[�D[� D\  D\}qD]�D]��D^�D^��D_�D_� D`�D`�Da�Da�Db�Db}qDb�qDc}qDc�qDd}qDe�De��De�qDf}qDf��Dg� Dg�qDhxRDh�qDi}qDi�qDj}qDk  Dk}qDk�qDl� Dm�Dm��Dn  Dn}qDn��Do� Do��Dp� Dq�Dq� Dr  Dr� Dr�qDs� Dt  Dt��DuDu��Dv�Dv}qDw  Dw� Dw�qDx��Dx�qDyz�Dy�qDz��D{�D{��D|�D|� D}D}� D~�D~�D  D��D�  D�=qD�}qD�� D�  D�>�D��HD��HD�  D�@ D�~�D��qD���D�AHD�� D�� D���D�>�D�� D�D��D�@ D�~�D��HD�  D�>�D�� D��HD�HD�@ D�� D�� D�  D�>�D�� D�� D�HD�@ D�� D���D��qD�@ D�� D���D���D�@ D���D���D���D�@ D�� D�D��D�AHD��HD���D�  D�AHD���D��HD�  D�@ D��HD�� D���D�>�D�� D��HD�  D�>�D�}qD���D�HD�@ D�� D���D�  D�AHD�� D���D�HD�@ D�~�D��HD�HD�@ D�� D��HD�HD�B�D��HD�� D���D�@ D�� D���D�HD�@ D�~�D���D�  D�>�D��HD�D�  D�@ D�� D��qD���D�@ D�� D�� D�HD�@ D�� D���D�  D�AHD�� D�� D���D�>�D�� D��HD�HD�AHD�� D��HD���D�>�D��HD��HD�  D�AHD�� D��qD���D�AHD�� D���D��qD�>�D�� D��HD�HD�@ D�~�D��qD���D�AHD�� D��HD�HD�AHD��HD��HD�  D�=qD�~�D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�}qD�� D�HD�>�D�~�D�� D�  D�@ D��HD���D���D�AHD�� D���D�  D�@ D�� D�� D�HD�AHD�� D�� D�HD�AHD�~�D���D�HD�@ D�}qD��qD���D�>�D�~�D���D��qD�=qD�}qD���D�HD�B�D���D��HD���D�@ D��HD�D�  D�>�D��HD�D���D�>�D��HD��HD��D�AHD��HD���D���D�@ D�~�D�� D�HD�AHD��HD�� D��qD�=qD�}qD�� D�HD�@ D�� D���D�HD�C�D��HD�� D�  D�@ D��HD�D��D�@ D�~�D���D�  D�B�D�� D���D�  D�@ D��HD�D��D�AHD�}qD¾�D�HD�@ D�~�D�� D�HD�B�DĂ�D��HD�  D�@ D�~�DŽqD���D�@ DƁHD�� D���D�>�D�~�D�� D���D�@ DȁHD�� D��qD�>�Dɀ Dɾ�D�  D�AHDʁHD�� D���D�@ Dˀ D˾�D�HD�AHD̀ D̽qD���D�@ D̀ D�� D���D�@ D΁HD�� D�  D�>�D�}qDϽqD���D�@ D�~�D�� D�  D�=qD�~�D��HD�HD�>�D�}qD�� D���D�=qDӁHD�� D�HD�B�DԀ D�� D�  D�AHDՂ�D��HD�  D�@ Dր D�� D��qD�>�D׀ D׼)D��)D�>�D؁HD��HD��D�AHD�}qDٽqD���D�@ DځHD�D��D�AHDہHD�� D���D�@ D�~�Dܾ�D�HD�B�D݂�D�D��D�AHDނ�D�� D�  D�AHD߂�D�D���D�@ D��HDྸD���D�>�D�~�D�� D��qD�>�D� D�� D�HD�AHD�HD��HD���D�>�D�~�D侸D��qD�>�D� D徸D�  D�@ D�~�D澸D���D�>�D� D��HD�HD�@ D�~�D�qD���D�AHD�HD�� D���D�@ D�HD꾸D��qD�>�D�~�D뾸D���D�>�D� D�� D�  D�@ D� D�� D�  D�AHD�HD�� D�  D�=qD�}qD�� D��D�AHD�� D��HD�HD�=qD� D�D�  D�@ D�}qD�D��D�C�D� D�� D�  D�>�D� D�� D���D�@ D�o\?#�
?W
=?�\)?�Q�?�@�@z�@.{@G�@W
=@h��@�G�@���@�33@�p�@�=q@�z�@�(�@�ff@У�@�p�@��@�{@��HA33A
=A(�A�A
=A�A   A%A*�HA/\)A2�\A8Q�A=p�A@��AEAK�AN{AR�\AXQ�A^{AaG�AfffAl��AqG�AuA{�A���A��A��A�\)A�=qA���A�ffA�G�A�z�A��RA�Q�A��
A��RA���A��HA�A���A��\A�p�A�Q�A��\A���A��A��HA�z�A��RA�=qA���AƸRAə�A�z�A�ffA�G�A�z�A�{A���A��
A�ffA�  A�33A�ffA��A�\A�A��A��HA���A�  A��HA��A�\)BG�B�\B�B��B�\B�B��B	�B�Bz�B��B33B��B��B�\BQ�B��BffB�BG�BffB�B��B=qB�B ��B!B#\)B$��B%��B'33B(��B)B*�HB,(�B-�B/33B0(�B1p�B3
=B4(�B5�B6�RB8(�B9�B:=qB;�B<��B>=qB?33B@��BB{BC
=BD(�BEBG
=BG�
BIG�BJ�RBK�
BL��BN=qBO�
BP��BQ�BS�BT��BU��BW
=BXz�BYp�BZ�\B\Q�B]p�B^ffB`  Bap�BbffBc�Bd��Bf�\Bg�Bhz�Bj{Bk\)Blz�Bmp�Bn�HBp(�Bq�BrffBs�
Bt��BuBw\)Bx��Byp�Bz�HB|Q�B}��B~�\B�
B��RB�G�B�B��\B�G�B��B�Q�B��B��B�(�B��HB���B�  B���B�p�B�{B�z�B�G�B�  B�z�B�
=B�B�z�B�
=B��B�Q�B�
=B��B�{B���B��B��B��RB�\)B��B�ffB�
=B��
B�ffB��HB��B�ffB���B�\)B�(�B���B�G�B��B���B��B��B�Q�B�
=B��B�(�B��RB�p�B�{B�z�B��B�B�z�B�
=B��B�(�B��HB��B�  B���B�\)B�  B�z�B��B��B��\B�
=B��B�z�B��B���B�=qB�
=B��B�=qB���B��B�Q�B���B�\)B�{B��HB��B��B���B�p�B�{B��\B�33B�  B��RB��B�B��\B��B���B�ffB�
=BÙ�B�(�B��HBř�B�{BƸRB�p�B��B�z�B��B��Bʣ�B��B�B̏\B�33BͮB�=qB�
=B�B�=qB���Bљ�B�=qBҸRB�p�B�(�BԸRB�G�B�  BָRB�\)B�B�ffB��B��
B�Q�B��HBۮB�=qB���B�p�B�(�B޸RB�33B��
B��\B�
=B�B�=qB���B�p�B��B��B�G�B��
B�Q�B���B�B�ffB��HB�p�B�(�B��HB�G�B��
B��B�G�B�B�Q�B�
=B�B�{B��B�p�B�  B�z�B�
=B�B�ffB���B�p�B��B���B�G�B��
B�=qB���B��B�(�B���B�G�B�B�z�B�33B��B�(�B���B��C {C Q�C �\C �CG�C�\C��C�C�C��C
=CQ�C�C  C\)C��C�HC33C�\C��C33Cz�C�
C33C�CC{Cp�C��C	(�C	p�C	�RC

=C
ffC
C{C\)C��C  C\)C�C��C=qC��C��C=qC�CC�C�C��C  C\)C�RC  CQ�C�\C�CG�C��C�C(�Cp�C��C33Cp�C�RC  CffC�RC  C=qC��C  C\)C��C�C33C��C��CG�C�C�
C�Cz�C�
C33Cz�CC
=Cp�C��C
=C\)C�C
=CffC�C��C=qCz�C�
C �C p�C ��C �HC!{C!ffC!��C!�HC"  C"33C"p�C"�C"�HC#
=C#(�C#Q�C#�C#�RC#�C${C$(�C$G�C$z�C$�C$�HC%  C%�C%33C%ffC%��C%��C%�HC&  C&(�C&ffC&�\C&C&�
C&��C'33C'ffC'�\C'�C'��C'��C((�C(ffC(�\C(�RC(�HC)  C)33C)ffC)��C)��C)�C*{C*=qC*p�C*�C*�HC+  C+(�C+ffC+��C+C+�C,�C,Q�C,�\C,�RC,�
C-  C-33C-ffC-��C-�
C.
=C.(�C.G�C.�C.C.�C/
=C/33C/p�C/�C/�HC0{C033C0\)C0��C0��C1
=C1�C1Q�C1�C1C2  C2(�C2Q�C2z�C2��C2�C3�C3G�C3p�C3��C3�
C4{C4G�C4p�C4��C4�RC4�C5(�C5ffC5�C5��C5�
C6{C6G�C6z�C6��C6C6�C7�C7\)C7�\C7�RC7�
C7��C833C8ffC8�C8��C8C8��C9(�C9\)C9�C9��C9C9�C:(�C:ffC:�C:�C:�
C;
=C;G�C;z�C;��C;C;�C<�C<\)C<��C<C<�
C=  C=33C=p�C=��C=�
C=�C>{C>Q�C>�C>�RC>�HC>��C?(�C?\)C?�\C?�RC?��C@  C@G�C@p�C@�C@�C@�HCA�CAQ�CAp�CA��CACB  CB=qCBffCB�CB��CB�
CC
=CCG�CCp�CC��CCCC�CD(�CDffCD�\CD�CD�CE�CEQ�CEp�CE��CE�
CF{CF=qCFffCF�CFCG  CG33CGQ�CGz�CG�RCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                ?�  @�@E�@�  @�G�@��R@�  AG�A  A�RA,(�A@  A_\)A�Q�A�Q�A�  A�  A���AУ�A�  A�\)A��B(�B  B�
B�B'�
B0  B7�
B@  BH(�BO�
BW�B`  Bh(�Bp  Bw�
B�  B�{B�  B��
B�  B�{B��B��B�{B��B�{B�{B�{B�{B�{B�(�B�  B��B�  B�(�B�(�B�{B�  B��B�  B�{B��B�  B�  B�{B�{B�{C   C  C  C��C��C
  C  C
=C  C��C��C  C
=C�C��C
=C {C"{C$  C&  C({C*
=C,  C.
=C/��C1�C4  C6
=C8
=C:  C<
=C>
=C@  CB  CC��CE�CG��CJ  CK��CM��CO��CR  CT
=CV
=CX  CZ  C\  C^  C_��Cb  Cd
=Ce��Ch  Cj
=Cl
=Cn
=Cp
=Cq��Cs�Cu�Cw�Cy��C{��C}��C�
=C�  C���C���C���C�  C�  C�  C�  C�  C���C�  C���C�  C���C���C�  C���C�  C�  C�C�  C�  C�  C���C�C�  C�C�
=C�C���C���C�C�  C�\C�C���C���C�C���C���C���C���C�  C�
=C�C�  C�  C���C�  C�  C�C�C�C�C�  C���C���C�  C�  C���C�C���C�  C�  C���C�  C�  C�  C���C���C���C�C�  C�  C�C�  C�  C�  C�C�  C�  C�
=C�\C�
=C���C�  C�C�C�  C�  C�  C�C�  C�  C�  C���C�  C���C�  C�C�  C�C�
=C�C���C�  C�  C�  C���C�  C���C�C���C�  C�C�C�C�C�C�C�  C�  C�C�
=C�C���C�  D �D � D  D� D  D� D�D� D�qD� D�D� D��Dz�D�qD}qD  D� D	�D	�D
�D
��D  Dz�D��D}qD  D��D�D��D  Dz�D�qD� D  D}qD�qD� D�D��D�D}qD  D� D�qD� D�qD}qD  D��D�D� D�D�D  D}qD�qD��DD��DD�DD� D��D � D!  D!}qD!�qD"}qD"�qD#� D#��D$}qD%D%��D%��D&}qD'D'��D'��D(}qD)D)��D)�qD*z�D+�D+��D+�qD,}qD,��D-z�D-��D.� D/  D/}qD0  D0}qD1�D1�D1�qD2}qD3  D3}qD4  D4� D4�qD5� D6�D6��D7  D7��D8D8��D9�D9� D9��D:� D:�qD;z�D<�D<��D<�qD=}qD>  D>}qD>��D?� D@�D@��DA�DA}qDA�qDB� DB��DC}qDD�DD�DE�DE��DF  DF}qDG  DG��DH�DH��DI�DI�DJ  DJ� DK�DK��DL  DL}qDL�qDM��DNDN��DO  DOz�DO�qDP��DQ  DQ� DR�DR� DS�DS�DT�DT� DU  DU}qDV  DV}qDV�qDW}qDW�qDX��DY  DY}qDY�qDZ� D[�D[� D\  D\}qD]�D]��D^�D^��D_�D_� D`�D`�Da�Da�Db�Db}qDb�qDc}qDc�qDd}qDe�De��De�qDf}qDf��Dg� Dg�qDhxRDh�qDi}qDi�qDj}qDk  Dk}qDk�qDl� Dm�Dm��Dn  Dn}qDn��Do� Do��Dp� Dq�Dq� Dr  Dr� Dr�qDs� Dt  Dt��DuDu��Dv�Dv}qDw  Dw� Dw�qDx��Dx�qDyz�Dy�qDz��D{�D{��D|�D|� D}D}� D~�D~�D  D��D�  D�=qD�}qD�� D�  D�>�D��HD��HD�  D�@ D�~�D��qD���D�AHD�� D�� D���D�>�D�� D�D��D�@ D�~�D��HD�  D�>�D�� D��HD�HD�@ D�� D�� D�  D�>�D�� D�� D�HD�@ D�� D���D��qD�@ D�� D���D���D�@ D���D���D���D�@ D�� D�D��D�AHD��HD���D�  D�AHD���D��HD�  D�@ D��HD�� D���D�>�D�� D��HD�  D�>�D�}qD���D�HD�@ D�� D���D�  D�AHD�� D���D�HD�@ D�~�D��HD�HD�@ D�� D��HD�HD�B�D��HD�� D���D�@ D�� D���D�HD�@ D�~�D���D�  D�>�D��HD�D�  D�@ D�� D��qD���D�@ D�� D�� D�HD�@ D�� D���D�  D�AHD�� D�� D���D�>�D�� D��HD�HD�AHD�� D��HD���D�>�D��HD��HD�  D�AHD�� D��qD���D�AHD�� D���D��qD�>�D�� D��HD�HD�@ D�~�D��qD���D�AHD�� D��HD�HD�AHD��HD��HD�  D�=qD�~�D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�}qD�� D�HD�>�D�~�D�� D�  D�@ D��HD���D���D�AHD�� D���D�  D�@ D�� D�� D�HD�AHD�� D�� D�HD�AHD�~�D���D�HD�@ D�}qD��qD���D�>�D�~�D���D��qD�=qD�}qD���D�HD�B�D���D��HD���D�@ D��HD�D�  D�>�D��HD�D���D�>�D��HD��HD��D�AHD��HD���D���D�@ D�~�D�� D�HD�AHD��HD�� D��qD�=qD�}qD�� D�HD�@ D�� D���D�HD�C�D��HD�� D�  D�@ D��HD�D��D�@ D�~�D���D�  D�B�D�� D���D�  D�@ D��HD�D��D�AHD�}qD¾�D�HD�@ D�~�D�� D�HD�B�DĂ�D��HD�  D�@ D�~�DŽqD���D�@ DƁHD�� D���D�>�D�~�D�� D���D�@ DȁHD�� D��qD�>�Dɀ Dɾ�D�  D�AHDʁHD�� D���D�@ Dˀ D˾�D�HD�AHD̀ D̽qD���D�@ D̀ D�� D���D�@ D΁HD�� D�  D�>�D�}qDϽqD���D�@ D�~�D�� D�  D�=qD�~�D��HD�HD�>�D�}qD�� D���D�=qDӁHD�� D�HD�B�DԀ D�� D�  D�AHDՂ�D��HD�  D�@ Dր D�� D��qD�>�D׀ D׼)D��)D�>�D؁HD��HD��D�AHD�}qDٽqD���D�@ DځHD�D��D�AHDہHD�� D���D�@ D�~�Dܾ�D�HD�B�D݂�D�D��D�AHDނ�D�� D�  D�AHD߂�D�D���D�@ D��HDྸD���D�>�D�~�D�� D��qD�>�D� D�� D�HD�AHD�HD��HD���D�>�D�~�D侸D��qD�>�D� D徸D�  D�@ D�~�D澸D���D�>�D� D��HD�HD�@ D�~�D�qD���D�AHD�HD�� D���D�@ D�HD꾸D��qD�>�D�~�D뾸D���D�>�D� D�� D�  D�@ D� D�� D�  D�AHD�HD�� D�  D�=qD�}qD�� D��D�AHD�� D��HD�HD�=qD� D�D�  D�@ D�}qD�D��D�C�D� D�� D�  D�>�D� D�� D���D�@ G�O�?#�
?W
=?�\)?�Q�?�@�@z�@.{@G�@W
=@h��@�G�@���@�33@�p�@�=q@�z�@�(�@�ff@У�@�p�@��@�{@��HA33A
=A(�A�A
=A�A   A%A*�HA/\)A2�\A8Q�A=p�A@��AEAK�AN{AR�\AXQ�A^{AaG�AfffAl��AqG�AuA{�A���A��A��A�\)A�=qA���A�ffA�G�A�z�A��RA�Q�A��
A��RA���A��HA�A���A��\A�p�A�Q�A��\A���A��A��HA�z�A��RA�=qA���AƸRAə�A�z�A�ffA�G�A�z�A�{A���A��
A�ffA�  A�33A�ffA��A�\A�A��A��HA���A�  A��HA��A�\)BG�B�\B�B��B�\B�B��B	�B�Bz�B��B33B��B��B�\BQ�B��BffB�BG�BffB�B��B=qB�B ��B!B#\)B$��B%��B'33B(��B)B*�HB,(�B-�B/33B0(�B1p�B3
=B4(�B5�B6�RB8(�B9�B:=qB;�B<��B>=qB?33B@��BB{BC
=BD(�BEBG
=BG�
BIG�BJ�RBK�
BL��BN=qBO�
BP��BQ�BS�BT��BU��BW
=BXz�BYp�BZ�\B\Q�B]p�B^ffB`  Bap�BbffBc�Bd��Bf�\Bg�Bhz�Bj{Bk\)Blz�Bmp�Bn�HBp(�Bq�BrffBs�
Bt��BuBw\)Bx��Byp�Bz�HB|Q�B}��B~�\B�
B��RB�G�B�B��\B�G�B��B�Q�B��B��B�(�B��HB���B�  B���B�p�B�{B�z�B�G�B�  B�z�B�
=B�B�z�B�
=B��B�Q�B�
=B��B�{B���B��B��B��RB�\)B��B�ffB�
=B��
B�ffB��HB��B�ffB���B�\)B�(�B���B�G�B��B���B��B��B�Q�B�
=B��B�(�B��RB�p�B�{B�z�B��B�B�z�B�
=B��B�(�B��HB��B�  B���B�\)B�  B�z�B��B��B��\B�
=B��B�z�B��B���B�=qB�
=B��B�=qB���B��B�Q�B���B�\)B�{B��HB��B��B���B�p�B�{B��\B�33B�  B��RB��B�B��\B��B���B�ffB�
=BÙ�B�(�B��HBř�B�{BƸRB�p�B��B�z�B��B��Bʣ�B��B�B̏\B�33BͮB�=qB�
=B�B�=qB���Bљ�B�=qBҸRB�p�B�(�BԸRB�G�B�  BָRB�\)B�B�ffB��B��
B�Q�B��HBۮB�=qB���B�p�B�(�B޸RB�33B��
B��\B�
=B�B�=qB���B�p�B��B��B�G�B��
B�Q�B���B�B�ffB��HB�p�B�(�B��HB�G�B��
B��B�G�B�B�Q�B�
=B�B�{B��B�p�B�  B�z�B�
=B�B�ffB���B�p�B��B���B�G�B��
B�=qB���B��B�(�B���B�G�B�B�z�B�33B��B�(�B���B��C {C Q�C �\C �CG�C�\C��C�C�C��C
=CQ�C�C  C\)C��C�HC33C�\C��C33Cz�C�
C33C�CC{Cp�C��C	(�C	p�C	�RC

=C
ffC
C{C\)C��C  C\)C�C��C=qC��C��C=qC�CC�C�C��C  C\)C�RC  CQ�C�\C�CG�C��C�C(�Cp�C��C33Cp�C�RC  CffC�RC  C=qC��C  C\)C��C�C33C��C��CG�C�C�
C�Cz�C�
C33Cz�CC
=Cp�C��C
=C\)C�C
=CffC�C��C=qCz�C�
C �C p�C ��C �HC!{C!ffC!��C!�HC"  C"33C"p�C"�C"�HC#
=C#(�C#Q�C#�C#�RC#�C${C$(�C$G�C$z�C$�C$�HC%  C%�C%33C%ffC%��C%��C%�HC&  C&(�C&ffC&�\C&C&�
C&��C'33C'ffC'�\C'�C'��C'��C((�C(ffC(�\C(�RC(�HC)  C)33C)ffC)��C)��C)�C*{C*=qC*p�C*�C*�HC+  C+(�C+ffC+��C+C+�C,�C,Q�C,�\C,�RC,�
C-  C-33C-ffC-��C-�
C.
=C.(�C.G�C.�C.C.�C/
=C/33C/p�C/�C/�HC0{C033C0\)C0��C0��C1
=C1�C1Q�C1�C1C2  C2(�C2Q�C2z�C2��C2�C3�C3G�C3p�C3��C3�
C4{C4G�C4p�C4��C4�RC4�C5(�C5ffC5�C5��C5�
C6{C6G�C6z�C6��C6C6�C7�C7\)C7�\C7�RC7�
C7��C833C8ffC8�C8��C8C8��C9(�C9\)C9�C9��C9C9�C:(�C:ffC:�C:�C:�
C;
=C;G�C;z�C;��C;C;�C<�C<\)C<��C<C<�
C=  C=33C=p�C=��C=�
C=�C>{C>Q�C>�C>�RC>�HC>��C?(�C?\)C?�\C?�RC?��C@  C@G�C@p�C@�C@�C@�HCA�CAQ�CAp�CA��CACB  CB=qCBffCB�CB��CB�
CC
=CCG�CCp�CC��CCCC�CD(�CDffCD�\CD�CD�CE�CEQ�CEp�CE��CE�
CF{CF=qCFffCF�CFCG  CG33CGQ�CGz�CG�RCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aا�A؍PA�x�A�v�A�p�A�`BA�^5A�VA�M�A�A�A�C�A�9XA�&�A�JA�%A���A��A��mA��TA��TA��;A��HA��HA��HA��HA��HA��;A��;A��;A��;A��;A��#A��/A��/A��/A��/A��/A��;A��/A��/A��/A��/A��;A��/A��/A��;A��;A��HA��HA��HA��;A��;A��/A��/A��;A��/A��/A��
A�ȴAײ-Aן�Aׇ+A��A�r�AԬAӃA�$�A�5?A�bNA�?}A�5?A̬A�x�AʋDA�VA�%A��A�ȴA���AǏ\A�oA�A�VA� �Aô9A�^5A�bNA��9A���A�VA�M�A���A���A�/A�dZA�z�A�oA�VA��wA�Q�A�JA�S�A���A�^5A�G�A��wA��/A�=qA���A���A��A�VA���A�bNA�E�A�A���A�-A�E�A���A�bNA�ȴA�^5A�1A���A�
=A��A���A���A���AhsA}l�Az��AvJAq�AnQ�Ail�AahsA_XA\��AZ�yAZM�AWAS"�APbAL�AK�hAJ9XAH^5AD�AB�A@ȴA?��A?�A>bNA;VA8E�A7&�A5?}A4bA3�FA3��A333A0�DA.z�A-��A-O�A,ȴA,bNA*�`A(�9A&�uA%�^A$��A$^5A#x�A"��A"�A"jA"=qA"1'A"E�A!\)A�#A�hA\)AĜA�uA�A ��A ��A!&�A!dZA!��A"�uA z�A�TA|�A"�A�jA�RA�!AA�AZA%Ax�A
�A	�A�jA�#A��A�AO�A�!A~�AI�A�A�7A��A�+A-A�A ��A ff@�ȴ@���@��@�1'@��\@��@��@�`B@��@�;d@�p�@�ƨ@�\)@�F@���@�@�?}@���@�&�@���@�1'@�1@� �@��;@���@���@�9X@�"�@��y@�~�@��@�X@�%@��@��`@���@�h@��@�j@�+@��/@���@��@띲@��@�9X@�1'@�dZ@��@��@�@�7L@�z�@��@�^5@�bN@�(�@��@�1@�l�@�bN@އ+@���@ܓu@�33@��y@���@���@�V@؛�@���@ٺ^@ّh@�%@��@�j@�+@��H@�"�@��@�ȴ@�+@�+@�C�@�@�@��`@мj@���@Гu@�j@�1'@���@ύP@ΰ!@�M�@��@ͩ�@���@�1'@˕�@�"�@��@��@ʰ!@�ff@�@ɑh@�G�@ȣ�@��;@Ǿw@ǅ@�;d@��y@�E�@�@Ł@�G�@��@��@öF@��y@���@�V@��^@�&�@�Ĝ@�A�@�1@���@�5?@�J@���@���@��#@���@�7L@���@���@��u@�r�@�9X@�t�@��y@���@�n�@�J@�@�7L@��`@���@���@�Z@�b@��F@�|�@�\)@�;d@���@��+@��@�@��@���@�p�@���@���@�Z@�ƨ@�+@���@�ȴ@��!@���@���@�5?@���@�V@��@�I�@�  @��;@��@�|�@�33@��H@��+@�E�@�{@�@�O�@��`@���@�Ĝ@��9@�bN@�ƨ@��@�;d@���@��R@��\@�$�@���@�X@�7L@��@���@�r�@��@��
@�+@���@��R@��\@�^5@�-@��#@��@�V@��D@� �@��F@�C�@��H@�n�@��@��^@�X@���@�Q�@��@��P@�C�@�@�ff@�=q@�{@��T@��@�&�@�V@��/@��@���@��D@�bN@��m@��@���@�v�@�5?@�J@���@��@�X@�%@�z�@� �@��m@��w@��P@�\)@�33@�v�@�{@�@��@��h@�&�@��9@���@��D@�bN@���@�t�@�"�@��y@���@�5?@��@���@�x�@�V@��@��/@�Ĝ@�j@��@��F@�l�@�"�@�
=@�ȴ@���@���@��R@��!@���@�~�@�-@��@�p�@�V@��9@���@��u@�Z@�(�@�1@��@���@��F@�|�@�ȴ@���@�~�@�V@�J@�x�@��@��@�Ĝ@���@�z�@�Z@�1'@�b@��@�C�@�@���@��R@��!@�~�@�{@��^@�hs@�O�@�O�@�G�@�&�@���@��j@��D@�1@��;@���@�S�@��@���@��\@�~�@�M�@��@���@�hs@���@���@��@�  @�@��@l�@��@�w@+@}�@}?}@|��@{�
@{�
@{�F@{�@{C�@z�!@z�\@zJ@y�7@y�@z�@y�#@y�7@yX@x��@w�w@w;d@vȴ@v@uO�@tI�@sdZ@r�\@r~�@rM�@q��@q��@qx�@p��@pA�@pA�@p �@o��@o+@nȴ@n�+@n$�@mp�@l�/@l�@lz�@k��@k�@ko@j�H@j�@i�@h��@h��@g�@g�@fv�@f$�@e�@e�h@e/@d��@d��@d�@c��@cS�@b��@b�\@b^5@b-@a�7@ahs@ahs@aG�@a%@`��@`�@`1'@_�P@^�R@^v�@^ff@^E�@]�-@\�@\j@\j@\(�@[�m@[��@[C�@Z�\@Z-@Y��@Y�@Y��@YX@X��@XbN@X  @W��@W�P@W;d@V�+@U�@UV@TI�@T1@S��@S33@SC�@S33@R��@Rn�@Q��@Q�^@Q�7@QX@Q&�@Q%@P�`@PĜ@P��@O�;@OK�@O\)@O;d@Nv�@N$�@N5?@M�@M?}@L�D@Lz�@L9X@K�
@Kt�@K33@J��@J^5@I��@I��@I&�@H�@Hb@G�w@G+@G
=@F��@Fȴ@F��@Fv�@F{@E�T@E�-@E�@E�@E/@D�j@DZ@D9X@C��@C�@CdZ@CC�@B��@Bn�@B-@B�@B�@BJ@A�@A��@@��@@��@@1'@?l�@>��@>�R@>��@=�T@=�h@=V@<�@<(�@;ƨ@;�F@;o@:~�@:J@9�@9hs@9&�@8��@8bN@8Q�@81'@7�;@7K�@6�y@6�@6ȴ@6��@6�+@6ff@6$�@5�-@5�@4��@4j@4I�@4�@3�F@2�H@1�#@1��@1G�@1&�@1&�@1&�@1�@0�`@0�9@0Q�@01'@/�;@/�P@.�y@.@-��@-@-��@-�@-V@,�D@,j@,(�@,1@+��@+�m@+�F@+dZ@+"�@*�!@*n�@*=q@*�@)�@)��@)&�@)%@(�`@(�9@(�@(�@(bN@(1'@'�;@'l�@'�@&��@&�+@&@%��@%p�@%O�@$��@$��@$(�@#�m@#ƨ@#��@#dZ@#S�@#o@"��@"��@"~�@"n�@"^5@!�@!�7@!x�@!G�@!&�@!%@ ��@ Ĝ@ �9@ ��@ �u@ r�@ bN@ b@�;@|�@�@��@E�@$�@@�h@`B@�@�D@�D@Z@9X@(�@��@ƨ@�@33@o@@��@~�@M�@�@�@��@��@��@�7@hs@7L@�`@��@�9@�u@r�@bN@bN@Q�@A�@1'@��@\)@;d@+@+@�@�@��@$�@��@�-@��@p�@�@��@z�@I�@�@��@�m@�
@�
@�F@�@C�@@�H@��@�\@M�@-@-@J@�^@X@7L@&�@�@��@��@��@�u@�u@r�@Q�@ �@�;@��@�w@�@��@�P@l�@+@
=@ȴ@�R@��@��@�+@v�Aة�Aا�Aا�Aا�A؛�A؉7A؁A�|�A�z�A�hsA�n�A�x�A�|�A�x�A�r�A�l�A�jA�jA�VA�ZA�bNA�\)A�^5A�^5A�XA�O�A�K�A�M�A�M�A�E�A�=qA�A�A�E�A�?}A�A�A�=qA�;dA�33A�9XA�33A�9XA�5?A�5?A�/A�+A�{A�bA�VA�JA�
=A�JA�VA�
=A�
=A�JA�1A�A�A�%A�A���A���A�  A���A��A���A���A��A��A���A��A��A��A��A��mA��`A��yA��`A��HA��mA��`A��HA��TA��`A��HA��HA��`A��TA��;A��TA��`A��HA��;A��HA��TA��/A��/A��HA��TA��;A��;A��TA��TA��/A��HA��`A��;A��/A��;A��TA��;A��;A��TA��`A��;A��/A��HA��TA��;A��;A��TA��TA��;A��;A��TA��TA��;A��;A��TA��HA��/A��HA��HA��/A��#A��#A��HA��HA��/A��/A��HA��;A��#A��;A��TA��;A��/A��/A��HA��HA��/A��;A��HA��/A��/A��HA��HA��/A��/A��;A��/A��A��#A��;A��A��#A��;A��;A��A��/A��;A��#A��A��;A��;A��#A��/A��;A��/A��#A��/A��HA��/A��#A��/A��HA��/A��#A��;A��;A��#A��/A��HA��/A��#A��;A��HA��#A��/A��;A��;A��/A��/A��HA��;A��#A��/A��;A��#A��#A��;A��;A��A��/A��;A��#A��#A��;A��;A��#A��/A��;A��/A��A��/A��;A��/A��#A��/A��HA��/A��#A��HA��;A��#A��;A��HA��;A��#A��/A��HA��/A��A��;A��HA��/A��#A��;A��;A��#A��#A��HA��;A��#A��#A��;A��HA��/A��/A��HA��HA��;A��/A��HA��TA��HA��/A��;A��HA��HA��;A��;A��TA��TA��;A��;A��TA��TA��;A��;A��TA��TA��;A��/A��;A��TA��;A��/A��;A��TA��;A��/A��;A��HA��TA��/A��/A��HA��/A��#A��/A��;A��;A��#A��/A��HA��/A��#A��;A��HA��;A��#A��HA��HA��/A��;A��HA��/A��/A��/A��HA��HA��/A��/A��HA��/A��A��A��A��A���A���A���A���A���A���A���A���A׼jA׺^A׼jA׸RAװ!A׬A׮Aש�Aף�Aן�Aף�Aס�Aם�Aכ�Aכ�Aו�A׍PA׉7Aׇ+A�x�A�t�A�dZA�=qA�(�A��A�
=A���A��
A�jA��/A�Q�A�1A���A��`A��#A��#A�ƨAԛ�Aԗ�Aԏ\A�n�A�I�A�?}A�bAө�A��TAҡ�A�-Aѡ�A�I�A�/A�VA�A��/AЙ�A�z�A�G�A�;dA��A�VA�oA���A���Aϟ�A�S�A�33A�{A��A�ĜAΥ�A�p�A�C�A�
=A��;Aͺ^A�jA�S�A�?}A�(�A�oA���A��A��/A��
A���A̼jA�n�A�  A��
A˧�A�r�A�A�A�9XA�/A�1A���Aʣ�A�n�A�&�A�"�A��A��A�VA�%A�1A�JA�
=A�1A�%A�1A�%A�  A���A���A���A���A���A��A��#A��
A��/A���A�ȴA���Aɲ-AɓuA�l�A��;Aȣ�A�z�A�E�A���A�ĜAǬAǡ�AǑhA�jA�M�A�E�A�5?A�$�A�{A�
=A��A���AƑhA���AŃA�l�A�dZA�`BA�XA�Q�A�Q�A�Q�A�VA�\)A�^5A�S�A�=qA�&�A�A�ƨA�dZA�VA��;Aã�AÁA�`BA� �A��yA°!A�|�A�dZA�5?A��A��`A�ĜA���A��uA�t�A�VA�K�A�?}A�33A��A���A��mA���A���A���A���A��jA��!A���A���A��+A�x�A�^5A�?}A�33A�&�A��A���A��RA��DA�jA�C�A�(�A��A�A���A��HA���A���A��A��7A�^5A�/A�JA���A��mA��
A���A���A��DA�p�A�`BA�S�A�=qA�-A��A�{A�JA�  A���A��A��RA���A��7A�v�A�l�A�ffA�`BA�XA�S�A�K�A�?}A�&�A�bA��A���A��DA�ZA��A��
A���A��DA�jA�O�A�;dA�(�A�bA��A��HA��
A���A���A��DA�x�A�l�A�`BA�Q�A�K�A�7LA�$�A��A�%A��A��;A�ȴA��A��7A�^5A�A�A�VA�ƨA���A�~�A�jA�O�A�9XA�+A��A���A��;A��jA���A��hA�z�A�jA�XA�C�A�$�A�
=A���A��A��
A��FA���A���A���A�jA�7LA�  A���A��A���A���A�ZA�%A��/A�G�A��yA��uA�jA�ZA�A�A�$�A�bA�A���A��A���A���A�+A�ƨA�~�A�A��DA�C�A��A���A��A��A��A��/A���A���A�t�A�I�A�1'A�1A��HA���A��DA�|�A�hsA�M�A�;dA���A��+A�Q�A��A�%A��A��A��yA��HA��A���A���A�jA�9XA�A��A���A�x�A�Q�A�1'A���A�A�x�A�"�A��mA��9A��A���A��RA��A�K�A� �A��#A��!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                Aا�A؍PA�x�A�v�A�p�A�`BA�^5A�VA�M�A�A�A�C�A�9XA�&�A�JA�%A���A��A��mA��TA��TA��;A��HA��HA��HA��HA��HA��;A��;A��;A��;A��;A��#A��/A��/A��/A��/A��/A��;A��/A��/A��/A��/A��;A��/A��/A��;A��;A��HA��HA��HA��;A��;A��/A��/A��;A��/A��/A��
A�ȴAײ-Aן�Aׇ+A��A�r�AԬAӃA�$�A�5?A�bNA�?}A�5?A̬A�x�AʋDA�VA�%A��A�ȴA���AǏ\A�oA�A�VA� �Aô9A�^5A�bNA��9A���A�VA�M�A���A���A�/A�dZA�z�A�oA�VA��wA�Q�A�JA�S�A���A�^5A�G�A��wA��/A�=qA���A���A��A�VA���A�bNA�E�A�A���A�-A�E�A���A�bNA�ȴA�^5A�1A���A�
=A��A���A���A���AhsA}l�Az��AvJAq�AnQ�Ail�AahsA_XA\��AZ�yAZM�AWAS"�APbAL�AK�hAJ9XAH^5AD�AB�A@ȴA?��A?�A>bNA;VA8E�A7&�A5?}A4bA3�FA3��A333A0�DA.z�A-��A-O�A,ȴA,bNA*�`A(�9A&�uA%�^A$��A$^5A#x�A"��A"�A"jA"=qA"1'A"E�A!\)A�#A�hA\)AĜA�uA�A ��A ��A!&�A!dZA!��A"�uA z�A�TA|�A"�A�jA�RA�!AA�AZA%Ax�A
�A	�A�jA�#A��A�AO�A�!A~�AI�A�A�7A��A�+A-A�A ��A ff@�ȴ@���@��@�1'@��\@��@��@�`B@��@�;d@�p�@�ƨ@�\)@�F@���@�@�?}@���@�&�@���@�1'@�1@� �@��;@���@���@�9X@�"�@��y@�~�@��@�X@�%@��@��`@���@�h@��@�j@�+@��/@���@��@띲@��@�9X@�1'@�dZ@��@��@�@�7L@�z�@��@�^5@�bN@�(�@��@�1@�l�@�bN@އ+@���@ܓu@�33@��y@���@���@�V@؛�@���@ٺ^@ّh@�%@��@�j@�+@��H@�"�@��@�ȴ@�+@�+@�C�@�@�@��`@мj@���@Гu@�j@�1'@���@ύP@ΰ!@�M�@��@ͩ�@���@�1'@˕�@�"�@��@��@ʰ!@�ff@�@ɑh@�G�@ȣ�@��;@Ǿw@ǅ@�;d@��y@�E�@�@Ł@�G�@��@��@öF@��y@���@�V@��^@�&�@�Ĝ@�A�@�1@���@�5?@�J@���@���@��#@���@�7L@���@���@��u@�r�@�9X@�t�@��y@���@�n�@�J@�@�7L@��`@���@���@�Z@�b@��F@�|�@�\)@�;d@���@��+@��@�@��@���@�p�@���@���@�Z@�ƨ@�+@���@�ȴ@��!@���@���@�5?@���@�V@��@�I�@�  @��;@��@�|�@�33@��H@��+@�E�@�{@�@�O�@��`@���@�Ĝ@��9@�bN@�ƨ@��@�;d@���@��R@��\@�$�@���@�X@�7L@��@���@�r�@��@��
@�+@���@��R@��\@�^5@�-@��#@��@�V@��D@� �@��F@�C�@��H@�n�@��@��^@�X@���@�Q�@��@��P@�C�@�@�ff@�=q@�{@��T@��@�&�@�V@��/@��@���@��D@�bN@��m@��@���@�v�@�5?@�J@���@��@�X@�%@�z�@� �@��m@��w@��P@�\)@�33@�v�@�{@�@��@��h@�&�@��9@���@��D@�bN@���@�t�@�"�@��y@���@�5?@��@���@�x�@�V@��@��/@�Ĝ@�j@��@��F@�l�@�"�@�
=@�ȴ@���@���@��R@��!@���@�~�@�-@��@�p�@�V@��9@���@��u@�Z@�(�@�1@��@���@��F@�|�@�ȴ@���@�~�@�V@�J@�x�@��@��@�Ĝ@���@�z�@�Z@�1'@�b@��@�C�@�@���@��R@��!@�~�@�{@��^@�hs@�O�@�O�@�G�@�&�@���@��j@��D@�1@��;@���@�S�@��@���@��\@�~�@�M�@��@���@�hs@���@���@��@�  @�@��@l�@��@�w@+@}�@}?}@|��@{�
@{�
@{�F@{�@{C�@z�!@z�\@zJ@y�7@y�@z�@y�#@y�7@yX@x��@w�w@w;d@vȴ@v@uO�@tI�@sdZ@r�\@r~�@rM�@q��@q��@qx�@p��@pA�@pA�@p �@o��@o+@nȴ@n�+@n$�@mp�@l�/@l�@lz�@k��@k�@ko@j�H@j�@i�@h��@h��@g�@g�@fv�@f$�@e�@e�h@e/@d��@d��@d�@c��@cS�@b��@b�\@b^5@b-@a�7@ahs@ahs@aG�@a%@`��@`�@`1'@_�P@^�R@^v�@^ff@^E�@]�-@\�@\j@\j@\(�@[�m@[��@[C�@Z�\@Z-@Y��@Y�@Y��@YX@X��@XbN@X  @W��@W�P@W;d@V�+@U�@UV@TI�@T1@S��@S33@SC�@S33@R��@Rn�@Q��@Q�^@Q�7@QX@Q&�@Q%@P�`@PĜ@P��@O�;@OK�@O\)@O;d@Nv�@N$�@N5?@M�@M?}@L�D@Lz�@L9X@K�
@Kt�@K33@J��@J^5@I��@I��@I&�@H�@Hb@G�w@G+@G
=@F��@Fȴ@F��@Fv�@F{@E�T@E�-@E�@E�@E/@D�j@DZ@D9X@C��@C�@CdZ@CC�@B��@Bn�@B-@B�@B�@BJ@A�@A��@@��@@��@@1'@?l�@>��@>�R@>��@=�T@=�h@=V@<�@<(�@;ƨ@;�F@;o@:~�@:J@9�@9hs@9&�@8��@8bN@8Q�@81'@7�;@7K�@6�y@6�@6ȴ@6��@6�+@6ff@6$�@5�-@5�@4��@4j@4I�@4�@3�F@2�H@1�#@1��@1G�@1&�@1&�@1&�@1�@0�`@0�9@0Q�@01'@/�;@/�P@.�y@.@-��@-@-��@-�@-V@,�D@,j@,(�@,1@+��@+�m@+�F@+dZ@+"�@*�!@*n�@*=q@*�@)�@)��@)&�@)%@(�`@(�9@(�@(�@(bN@(1'@'�;@'l�@'�@&��@&�+@&@%��@%p�@%O�@$��@$��@$(�@#�m@#ƨ@#��@#dZ@#S�@#o@"��@"��@"~�@"n�@"^5@!�@!�7@!x�@!G�@!&�@!%@ ��@ Ĝ@ �9@ ��@ �u@ r�@ bN@ b@�;@|�@�@��@E�@$�@@�h@`B@�@�D@�D@Z@9X@(�@��@ƨ@�@33@o@@��@~�@M�@�@�@��@��@��@�7@hs@7L@�`@��@�9@�u@r�@bN@bN@Q�@A�@1'@��@\)@;d@+@+@�@�@��@$�@��@�-@��@p�@�@��@z�@I�@�@��@�m@�
@�
@�F@�@C�@@�H@��@�\@M�@-@-@J@�^@X@7L@&�@�@��@��@��@�u@�u@r�@Q�@ �@�;@��@�w@�@��@�P@l�@+@
=@ȴ@�R@��@��@�+G�O�Aة�Aا�Aا�Aا�A؛�A؉7A؁A�|�A�z�A�hsA�n�A�x�A�|�A�x�A�r�A�l�A�jA�jA�VA�ZA�bNA�\)A�^5A�^5A�XA�O�A�K�A�M�A�M�A�E�A�=qA�A�A�E�A�?}A�A�A�=qA�;dA�33A�9XA�33A�9XA�5?A�5?A�/A�+A�{A�bA�VA�JA�
=A�JA�VA�
=A�
=A�JA�1A�A�A�%A�A���A���A�  A���A��A���A���A��A��A���A��A��A��A��A��mA��`A��yA��`A��HA��mA��`A��HA��TA��`A��HA��HA��`A��TA��;A��TA��`A��HA��;A��HA��TA��/A��/A��HA��TA��;A��;A��TA��TA��/A��HA��`A��;A��/A��;A��TA��;A��;A��TA��`A��;A��/A��HA��TA��;A��;A��TA��TA��;A��;A��TA��TA��;A��;A��TA��HA��/A��HA��HA��/A��#A��#A��HA��HA��/A��/A��HA��;A��#A��;A��TA��;A��/A��/A��HA��HA��/A��;A��HA��/A��/A��HA��HA��/A��/A��;A��/A��A��#A��;A��A��#A��;A��;A��A��/A��;A��#A��A��;A��;A��#A��/A��;A��/A��#A��/A��HA��/A��#A��/A��HA��/A��#A��;A��;A��#A��/A��HA��/A��#A��;A��HA��#A��/A��;A��;A��/A��/A��HA��;A��#A��/A��;A��#A��#A��;A��;A��A��/A��;A��#A��#A��;A��;A��#A��/A��;A��/A��A��/A��;A��/A��#A��/A��HA��/A��#A��HA��;A��#A��;A��HA��;A��#A��/A��HA��/A��A��;A��HA��/A��#A��;A��;A��#A��#A��HA��;A��#A��#A��;A��HA��/A��/A��HA��HA��;A��/A��HA��TA��HA��/A��;A��HA��HA��;A��;A��TA��TA��;A��;A��TA��TA��;A��;A��TA��TA��;A��/A��;A��TA��;A��/A��;A��TA��;A��/A��;A��HA��TA��/A��/A��HA��/A��#A��/A��;A��;A��#A��/A��HA��/A��#A��;A��HA��;A��#A��HA��HA��/A��;A��HA��/A��/A��/A��HA��HA��/A��/A��HA��/A��A��A��A��A���A���A���A���A���A���A���A���A׼jA׺^A׼jA׸RAװ!A׬A׮Aש�Aף�Aן�Aף�Aס�Aם�Aכ�Aכ�Aו�A׍PA׉7Aׇ+A�x�A�t�A�dZA�=qA�(�A��A�
=A���A��
A�jA��/A�Q�A�1A���A��`A��#A��#A�ƨAԛ�Aԗ�Aԏ\A�n�A�I�A�?}A�bAө�A��TAҡ�A�-Aѡ�A�I�A�/A�VA�A��/AЙ�A�z�A�G�A�;dA��A�VA�oA���A���Aϟ�A�S�A�33A�{A��A�ĜAΥ�A�p�A�C�A�
=A��;Aͺ^A�jA�S�A�?}A�(�A�oA���A��A��/A��
A���A̼jA�n�A�  A��
A˧�A�r�A�A�A�9XA�/A�1A���Aʣ�A�n�A�&�A�"�A��A��A�VA�%A�1A�JA�
=A�1A�%A�1A�%A�  A���A���A���A���A���A��A��#A��
A��/A���A�ȴA���Aɲ-AɓuA�l�A��;Aȣ�A�z�A�E�A���A�ĜAǬAǡ�AǑhA�jA�M�A�E�A�5?A�$�A�{A�
=A��A���AƑhA���AŃA�l�A�dZA�`BA�XA�Q�A�Q�A�Q�A�VA�\)A�^5A�S�A�=qA�&�A�A�ƨA�dZA�VA��;Aã�AÁA�`BA� �A��yA°!A�|�A�dZA�5?A��A��`A�ĜA���A��uA�t�A�VA�K�A�?}A�33A��A���A��mA���A���A���A���A��jA��!A���A���A��+A�x�A�^5A�?}A�33A�&�A��A���A��RA��DA�jA�C�A�(�A��A�A���A��HA���A���A��A��7A�^5A�/A�JA���A��mA��
A���A���A��DA�p�A�`BA�S�A�=qA�-A��A�{A�JA�  A���A��A��RA���A��7A�v�A�l�A�ffA�`BA�XA�S�A�K�A�?}A�&�A�bA��A���A��DA�ZA��A��
A���A��DA�jA�O�A�;dA�(�A�bA��A��HA��
A���A���A��DA�x�A�l�A�`BA�Q�A�K�A�7LA�$�A��A�%A��A��;A�ȴA��A��7A�^5A�A�A�VA�ƨA���A�~�A�jA�O�A�9XA�+A��A���A��;A��jA���A��hA�z�A�jA�XA�C�A�$�A�
=A���A��A��
A��FA���A���A���A�jA�7LA�  A���A��A���A���A�ZA�%A��/A�G�A��yA��uA�jA�ZA�A�A�$�A�bA�A���A��A���A���A�+A�ƨA�~�A�A��DA�C�A��A���A��A��A��A��/A���A���A�t�A�I�A�1'A�1A��HA���A��DA�|�A�hsA�M�A�;dA���A��+A�Q�A��A�%A��A��A��yA��HA��A���A���A�jA�9XA�A��A���A�x�A�Q�A�1'A���A�A�x�A�"�A��mA��9A��A���A��RA��A�K�A� �A��#A��!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B3�B3�B2�B1�B3�B2�B1'B2-B1�B2-B0�B1'B2-B1'B1'B1'B1'B1'B0�B0�B1[B0�B1'B1�B1'B1[B1�B1[B1�B1�B1�B2-B1�B1�B1�B1�B2aB1�B2-B2aB2�B2aB2-B2�B2�B2�B2�B2�B33B33B3�B4B4�B4�B4�B5?B5B5?B5?B33B1�B.IB'�B$B�B
��B
� B
��B
��B
��B
��B
�gB
�'B
�)B
уB
�aB
��B
��B
��B
�B
��BB�B�B2aB@�BP�BW
Bo5B�"B�RB��B�B�mB��B�B�B�[BȴBٴB�B�AB��B��B1B.B�B�BB{B�B�B�_B�4B�uBzxBr�BbNB?�B3�B�B
�B
�EB
��B
�6B
��B
�B
{�B
v�B
`�B
Q�B
?�B
2�B
�B
B	��B	��B	�zB	�?B	��B	�XB	�4B	�kB	��B	u�B	f2B	]�B	U2B	MjB	C-B	4�B	2-B	*eB	'�B	$@B	�B	�B	�B	AB��B��B��B�B��B��B��B�jBںB��B֡B�vB��B�BƨB�tBȀB��B�jB�BуB�aB�B��B��B��B	 iB		7B	.B	B	^jB	k�B	p�B	wfB	��B	��B	��B	�OB	�B	�kB	��B	�eB	��B	��B	K�B	B�B	8RB	�B	�B	{B	 iB	B	 �B	 iB	B	�B	�B		�B	B	oB	eB		B	�B	1B	�B	�B	!-B	�B	7B	�B	=B	�B	B	!�B	+kB	/OB	1'B	6�B	8�B	B[B	LdB	L�B	LdB	PHB	[WB	a|B	_�B	bNB	e`B	pB	x�B	.B	��B	��B	�4B	�0B	��B	��B	�qB	�=B	��B	�B	�3B	�B	�dB	��B	��B	��B	̘B	�XB	�}B	�gB	ٴB	�KB	�B	��B	�WB	��B	�B	�|B	�B	��B	�NB	��B	�NB	�B	چB	уB	��B	��B	уB	��B	� B	� B	� B	уB	�TB	��B	��B	ٴB	�aB	�jB	�pB	͟B	�[B	��B	��B	�NB	�B	�&B	�B	�&B	�NB	�mB	��B	��B	�B	� B	�B	�vB	�B	�B	�B	��B	��B	��B	��B	��B	�`B	�2B	�>B	�"B	��B	��B	��B
�B
�B
B
B
GB
MB
�B
�B
B
�B
B
�B
	B

	B

�B
JB
xB
~B
PB
DB
�B

rB
�B
�B

=B

�B
bB
�B
uB
�B
B
B
�B
�B
�B
B
 B
.B
�B
�B
�B
�B
 B
hB
�B
4B
hB
B
�B
�B
:B
B
�B
�B
�B
�B
B
�B
�B
$B
�B
_B
�B
1B
B
	B
�B
B
�B
IB
B
VB
�B
�B
�B
�B
 'B
 �B
 �B
 \B
 �B
 �B
!�B
#B
#nB
#nB
$B
&B
%�B
&LB
%�B
&�B
'B
(�B
(�B
(�B
'�B
(XB
($B
(XB
)_B
(�B
)*B
)�B
*�B
*�B
,qB
,qB
-CB
-�B
.�B
.�B
0UB
0!B
1�B
1�B
2�B
4nB
4�B
4nB
5B
5�B
5�B
5�B
6�B
5�B
6zB
7LB
7LB
7LB
7�B
8�B
8�B
8�B
9$B
9�B
:�B
:�B
:�B
<6B
=qB
?�B
?HB
?}B
?HB
?�B
@B
?�B
A B
A�B
A�B
A�B
B'B
B[B
B[B
B�B
DgB
D3B
C�B
C�B
DgB
E�B
EmB
E�B
E�B
E�B
GzB
GzB
GzB
GzB
G�B
H�B
H�B
IB
I�B
J�B
J�B
JXB
J�B
J�B
J�B
K^B
LdB
K�B
K�B
M�B
NpB
NpB
N�B
N�B
N�B
O�B
QNB
P�B
Q�B
PHB
QB
P}B
QNB
RTB
R�B
R�B
R�B
R�B
R�B
R�B
TaB
T,B
S�B
S�B
TaB
T�B
T,B
TaB
T�B
T�B
T�B
T�B
U2B
T�B
UgB
V9B
VmB
VmB
V9B
VB
VB
WsB
W?B
W?B
W?B
W
B
V�B
WsB
W?B
W
B
W�B
W�B
WsB
W�B
X�B
YB
YB
ZB
Z�B
[#B
[�B
[�B
\�B
\�B
]/B
]/B
[�B
[�B
[�B
\�B
^B
_pB
^jB
]dB
\�B
^B
]/B
]/B
]dB
]dB
]�B
]�B
]dB
]�B
^jB
a�B
c�B
d&B
dZB
d�B
dZB
e,B
e�B
e�B
e�B
e�B
ffB
f�B
ffB
ffB
g8B
g�B
hsB
h>B
hsB
h
B
h
B
iB
jB
jB
jKB
i�B
i�B
i�B
i�B
i�B
j�B
kQB
k�B
j�B
kB
k�B
lWB
k�B
k�B
l�B
m]B
m�B
m�B
m�B
n�B
oiB
o�B
o�B
p;B
p�B
qB
qvB
qvB
q�B
q�B
r�B
r|B
r|B
r�B
r�B
r�B
sB
r�B
sMB
s�B
s�B
tTB
t�B
t�B
t�B
u�B
v`B
v`B
v�B
v�B
wfB
w�B
xB
w�B
w�B
w�B
w�B
xB
w�B
w2B
w�B
xB
w�B
x8B
w�B
xlB
w�B
w�B
w�B
w�B
xB
x8B
y�B
x�B
y>B
y�B
zB
y�B
y�B
y�B
zB
zB
z�B
{�B
{�B
{�B
{�B
|PB
{�B
|B
|�B
|�B
{B
{�B
|B
|B
|B
|B
|�B
|�B
|�B
|�B
}VB
}�B
}�B
~(B
~]B
~]B
~]B
~�B
~�B
~�B
.B
.B
�B
�B
cB
�B
�4B
�iB
�4B
��B
�;B
�B
�;B
��B
�B
�B
�B
�B
�B
�B
�uB
�B
�B
��B
�MB
�SB
�SB
��B
��B
��B
�_B
��B
��B
�7B
��B
�	B
�	B
�rB
�=B
�=B
��B
�DB
�~B
�B
��B
��B
�\B
�\B
�\B
�\B
��B
��B
��B
��B
� B
� B
�hB
��B
��B
�hB
�hB
�uB
��B
�B
��B
��B
��B
��B
��B
��B
�B
�B
�MB
��B
��B
��B
��B
��B
��B
��B
��B
�YB
��B
��B
��B
��B
��B
��B
��B
�_B
��B
��B
�1B
�eB
�eB
��B
�B
�kB
�kB
�kB
��B
��B
�	B
�	B
�=B
��B
��B
�CB
�CB
�xB
�B
�IB
�IB
�~B
��B
�B
��B
��B
��B
��B
�!B
�!B
��B
��B
��B
��B
��B
��B
��B
�-B
��B
�-B
�-B
��B
��B
�bB
�bB
�bB
�-B
��B
�bB
��B
��B
�B
�nB
��B
��B
��B
�@B
�@B
�@B
�FB
�zB
�FB
��B
�zB
��B
��B
�B
��B
��B
��B
��B
�B
�B
�RB
��B
��B
��B
��B
��B
��B
��B
��B
�$B
��B
�$B
�XB
��B
�*B
��B
�*B
��B
��B
��B
�0B
�0B
�0B
��B
�0B
�eB
��B
�6B
�kB
�6B
�6B
��B
�B
�qB
�=B
��B
��B
�B
��B
��B
��B
��B
�wB
��B
��B
�B
�B
�}B
��B
�}B
�IB
�}B
��B
�OB
�OB
�OB
�OB
��B
��B
��B
��B
��B
��B
��B
�!B
��B
��B
��B
��B
��B
��B
��B
�'B
�[B
��B
��B
��B
��B
��B
��B5B5?B2�B2�B4�B5�B2aB1�B49B7LB1[B1�B2-B2�B5?B2�B1[B4�B49B1[B.�B1[B2�B1�B.�B2aB3�B/OB1�B1�B3�B1'B0�B0!B3hB0�B/�B2�B0�B/�B.}B2�B/B5�B3hB:^B0!B1'B1�B2-B0�B/�B1[B1�B0�B0�B2�B0�B/OB1'B2aB0�B/�B1�B1�B0�B0UB1�B1[B0UB0!B2aB1'B/�B1�B1�B/�B0�B2-B0!B0!B2-B1[B/�B1�B1�B0!B0�B2-B1'B/�B1'B2-B1'B0!B1�B1�B0�B/�B1'B1�B0!B0�B2-B1[B0!B1[B2-B1[B0!B1�B1�B0UB0!B1�B2-B0�B0�B2aB1�B0�B0�B1�B2-B0�B0�B1�B1�B0�B1'B2�B1[B0�B1�B2aB2�B0!B0�B2aB2-B0�B1'B2�B1�B0!B1�B2�B2aB0�B1'B2�B1�B0�B2aB2�B1'B0�B2�B2aB1'B1�B2�B2aB0�B2�B2�B0�B1'B33B1�B1'B2aB2�B1[B1�B2�B2aB0�B2-B2�B2-B0�B2-B33B1�B1'B1�B33B1�B1'B2�B2aB0�B2-B3hB1�B1'B2�B2�B1�B1[B2�B2aB0�B1�B2�B1�B1[B2�B2�B1�B1�B3hB2-B1[B33B2�B1�B1�B33B2aB1'B2aB3hB2�B1�B2�B3�B2-B1[B2�B33B1'B2-B3hB1�B1�B2�B3�B33B1�B2-B4B1�B1�B2�B3�B2aB1�B3�B3hB1�B2�B4B3�B2aB1�B33B3�B1�B1�B3�B4B2�B1�B2�B4B33B2aB2�B3hB3�B2aB2-B3�B3�B2-B1�B4B3�B2-B2aB4B4nB3�B2aB4B5B4B2�B4B5?B4nB33B2�B5B4�B3hB49B5?B5B3�B4B5?B5tB4B5B6B4�B3�B5B6B49B3�B5�B5B49B5tB6B5�B4nB3�B5�B5�B4B4�B5�B5�B5?B49B6B6B5B4B6zB5tB3�B49B5�B49B2�B3hB49B49B2aB2aB33B3�B0�B0UB1�B0�B/�B/B/�B/OB,�B,qB,�B0UB+B(XB&LB%�B �B$�B3�B!�BBB	BB�B�BSB
��B
��B
��B iB
�JB
�8B
�BoBB
�B
��B
�B
��B
�5B
�pB
��B
�HB
�B
�QB
�2B
�aB
�,B
уB
�<B
�,B
уB
͟B
��B
��B
ȴB
��B
ƨB
� B
��B
��B
�}B
��B
��B
��B
�BB
�}B
��B
��B
�HB
��B
��B
��B
�}B
�-B
�pB
��B
�gB
��B
�KB
��B
�OB
��B
�B
�zB
ɆB
�6B
��B
��B
�pB
бB
� B
ҽB
��B
уB
�&B
��B
�aB
�[B
��B
�mB
רB
�EB
��B
רB
�
B
�B
�dB
ںB
��B
خB
�)B
ںB
ܒB
ٴB
ޞB
�5B
�EB
�B
ߤB
�B
�B
�8B
�2B
�2B
�B
�2B
��B
�"B
��B
��B
��B
��BB�B)*B@BbBJB	lB�B~B�BPBB�B�B�B�B �B&�B33B6�B5B/�B-CB.B1'B5�B;�B?�B8�B<jBB�BGEBI�BO�BM6BNBN�BP�BO�BS�BS�BRTBR�BT�BT,BQ�BP�BR�BU�BW?BZQBY�B]dB\]Bc�Bf2Be`Bd�Bd�Bh>Bu%BqBq�Bu�Bv�Bv�Bx�Bz�B}�B~�B�B�SB�DB��B��B��B�SB�+B�7B�~B��B��B�nB�FB��B�B��B�CB��B�B��B�OB�9B�zB��B��B��B��B�9B��B�?B�B�?B��B�B��B�nB��B��B�KBϫB��B�&B�[B�,B�2B��B��B�?B�QBچB�yBרB�EB֡B՛B��B�BB�B�B�B��B��B��B�B͟B�pB�<B�B�dBɺB�BB�B�EBŢB�mBŢB��B�gBǮB��B�BȴB��B��B�?B� B��B��B�3B�B��B� B�'B�B��B�B��BĜB�gBȀB�#BʌBĜBÖB�2B�6B��B��BרB�B�B�?B�B�B��B�?B��BרBٴB�dB�B��B��B��B�B�oB�/B�)B�yB�B��B��B��B�B�B�B�B��B�B��B��B�B��B�B�+B�B iB��B��B��B��B�	B��B�lB�ZB��B �B��B�]B��B��B iBuB��B��BSB�B
	B�B+B�BVB�BxB�B�B�B�BoG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021101601175620211016011756IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021102604003220211026040032QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021102604003220211026040032QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365320220126093653IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295620220204232956IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295620220204232956IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295620220204232956IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                
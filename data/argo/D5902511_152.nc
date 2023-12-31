CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-11-19T22:01:26Z creation; 2021-04-29T20:27:08Z DMQC;      
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
_FillValue        G�O�     `  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     `  d@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     `  �x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` g   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20201119220126  20210429202817  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_152                 6810_008521_152                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�H�M�@�H�M�11  @�H����$@�H����$@2=L�_�@2=L�_��e%�B�¤�e%�B�¤11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	?u?��H@=p�@xQ�@�(�@��R@�G�A ��A  A\)A*=qA>�RA^�RA�  A�  A��A��A��A�  A�Q�A�B   B�
B�
B(�B (�B'�B/�
B7�
B?�
BG�
BP  BX  B`  Bh(�Bp  Bw�B�  B�{B��B�  B�  B�  B��B�{B�  B�  B�  B�  B�  B�  B�{B�{B�  B�{B�  B��B��B�{B�{B�{B�  B�  B�{B�(�B�{B�  B�  B�  C 
=C
=C
=C  C��C
  C
=C  C  C
=C
=C
=C��C  C{C  C   C"{C${C&{C(  C*
=C,{C.
=C/��C2  C4  C6  C8
=C:
=C<  C>  C?��CA��CD
=CF
=CH
=CJ
=CL  CN  CO��CR  CT
=CU�CX  CZ
=C\
=C^{C`�Cb
=Cd  Ce��Cg�Cj  Ck��Cn  Cp  Cr  Cs��Cv  Cw�Cy��C{��C~  C�  C���C�  C���C���C���C���C���C���C�  C�  C���C�C�  C���C���C�  C�C�C�
=C�C�  C�  C�C�  C�  C�  C�  C�  C���C���C�  C���C�  C�C�C�C�  C�  C�  C�C���C�  C�  C�
=C�C�
=C�  C�  C���C���C�C���C�C���C���C�  C�  C�C���C�C�  C�  C�  C�C���C�C�C�
=C�C���C�  C�  C���C�  C�C�  C�  C���C���C�  C�  C�C���C�  C�{C�C���C��C���C���C���C���C���C���C�  C���C���C�  C�  C���C���C�  C�
=C�C�C�C�  C�  C�C�C�  C�C�C���C�  C�  C�  C�  C�C�C���C�  C�C�C���C�  C�  D �D � D �qD� D  D}qD�qD}qD�D}qD�qD��D  D}qD�qD��D�qD� D	�D	��D	�qD
}qD
�qD� D�D}qD  D��D  D�D�D��D�D�D  Dz�D  D}qD  D� D  D}qD  D� D��D}qD�qD� D�qD� D�qD� D�qD� D  D� D��D}qD�qD}qD�qD� D  D� D   D ��D �qD!� D"  D"}qD#  D#}qD#��D$� D%  D%}qD&�D&� D'  D'� D'��D(z�D)  D)��D*�D*��D+  D+� D,�D,��D-D-��D-�qD.}qD.�qD/z�D/��D0}qD1  D1��D2D2�D3  D3}qD3�qD4z�D4�qD5}qD5��D6}qD6�qD7� D8�D8��D9D9��D:  D:}qD:�qD;z�D<  D<��D=�D=� D>D>�D?  D?� D?�qD@}qDADA��DB�DB� DC�DC�DD  DD� DE  DE��DF�DF�DG�DG}qDG��DH� DIDI��DJ  DJ� DK  DK}qDK�qDL� DL�qDM}qDN  DN��DO�DO�DP�DP� DP�qDQ� DR�DR� DS�DS� DS�qDT� DU  DU� DV  DV}qDV�qDWz�DW�qDX��DY�DY�DZ  DZ}qD[  D[� D[�qD\� D]�D]� D^  D^��D_�D_}qD_�qD`� D`�qDa� Db  Db}qDb�qDc� Dc�qDd}qDe�De� Df  Df��Dg�Dg� Dg�qDh}qDh�qDi}qDj  Dj� Dj�qDk� Dk��Dlz�Dl�qDmz�Dn  Dn��Dn�qDo� Dp�Dp��Dq  Dq� Dr�Dr�DsDs��Dt  Dt��Du�Du}qDv�Dv}qDw  Dw��Dx�Dx��Dx�qDy� Dz�Dz��D{�D{��D|�D|� D}�D}� D}�qD~�DD� D�qD�AHD��HD�� D���D�>�D�}qD�� D�HD�@ D�� D�� D�HD�B�D�� D���D���D�@ D��HD�� D���D�@ D�~�D���D�  D�AHD�� D�� D�  D�@ D��HD��HD�HD�AHD��HD���D���D�>�D�~�D�� D�HD�@ D�� D��HD���D�@ D��HD�� D�HD�@ D�� D��HD�  D�>�D�� D���D���D�AHD��HD��HD��D�AHD�� D��HD��D�AHD��HD�� D�  D�@ D��HD��HD���D�>�D�� D��HD�  D�=qD�}qD��qD��qD�=qD�}qD���D�  D�AHD�� D�� D�  D�@ D��HD��HD�HD�AHD���D��HD���D�@ D���D��HD�  D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�D���D��qD�>�D�� D��HD�HD�@ D�� D�� D��D�B�D�� D���D���D�AHD��HD�D��D�AHD�� D���D���D�>�D�� D�� D���D�>�D�� D��HD�HD�AHD�� D�� D���D�>�D�� D�� D�  D�@ D�� D��HD�  D�@ D�~�D�� D�HD�@ D�� D�� D�HD�@ D�� D���D�  D�AHD�~�D�� D���D�>�D�~�D�� D��D�B�D�� D�� D���D�>�D�~�D���D���D�@ D��HD�D�HD�AHD�� D�� D�HD�AHD��HD�D�HD�>�D�� D�� D�  D�AHD��HD���D�  D�@ D��HD���D�HD�>�D�}qD�� D�  D�>�D�� D�� D��qD�>�D�� D���D���D�@ D��HD�� D���D�AHD���D�� D�  D�AHD�~�D��HD�  D�=qD�~�D���D�  D�@ D���D�D�HD�AHD��HD�� D�HD�B�D��HD��HD�  D�@ D�� D�� D�  D�AHD��HD��qD���D�AHD��HD��HD�HD�@ D�� D�D��D�B�D���D�� D�HD�AHD D�� D�HD�AHD�}qD�� D��D�>�D�}qD�� D��D�AHD�~�Dž�D�  D�@ D�~�Dƾ�D�  D�AHDǀ D�� D��D�AHDȀ DȾ�D�HD�B�Dɀ D�� D���D�@ Dʀ D��HD�HD�AHDˁHD��HD�  D�>�D́HD�� D���D�>�D̀ D�D�  D�>�D�~�D��HD�HD�@ Dπ D�� D�  D�@ DЁHD�� D���D�=qD�}qDѾ�D�HD�AHD҂�D��HD�HD�@ D�~�D�� D�  D�AHDԁHD�� D���D�>�DՁHD�D�HD�AHDցHD��HD�  D�>�D�~�D��HD��D�AHD؁HD��HD�  D�@ Dـ D�� D�HD�@ D�~�D�� D��qD�@ DہHD۾�D�  D�AHD܁HD��HD�HD�@ D݀ D�� D���D�>�D�~�D�� D�HD�AHD߀ D�� D�HD�B�D���D�D��D�AHD� D��HD�  D�@ D� D��HD�HD�@ D� D�qD��qD�@ D� D�� D�  D�AHD�HD�� D�  D�>�D�HD�� D���D�@ D�HD�� D���D�>�D�~�D辸D�HD�AHD�}qD龸D�HD�@ D�HD��HD�  D�=qD�}qD�� D��D�@ D�~�D쾸D��qD�=qD�~�D�� D�  D�>�D�~�DD��D�B�D� D�� D�HD�AHD�� D�� D�  D�@ D�~�D�D�HD�B�D�HD�� D�  D�@ D� D��HD�  D�>�D�~�D���D���D�@ D���D��HD�  D�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�k�?�?W
=?�=q?���?�
=?��@\)@#�
@.{@E�@Y��@k�@z�H@��@�z�@��H@�ff@���@���@�G�@�{@�@�(�@�@�\)@�
=A�AA��A\)Az�AQ�A��A#33A&ffA*=qA0��A5�A7�A<��AB�\AG
=AJ=qAP��AU�AXQ�A^{Ac33AfffAj=qAp��As�
AxQ�A}p�A�  A��\A��A��RA���A��A��A��A��A��
A�A�Q�A��HA���A�
=A��A��A�p�A���A��HA�z�A�\)A�G�A��HA�p�A�  A���A�33A�{A��A���AÅA�A�
=A���A��
A�p�A�
=Aљ�A��
A�p�A�  A��AۅA�p�A�  AᙚA��HA��A�\)A�Q�A��A�(�A�A�
=A�G�A�33A�z�A�ffA���A�=qA��A�{A��B z�Bp�B�\B\)B  BG�B=qB�HB�B��B	B
ffB\)Bz�B��B{B
=B�BQ�Bp�BffB�HB�
B��Bp�B=qB\)BQ�B��B��B�HB�B  B�B�BffB�B Q�B ��B!��B"�\B#
=B#�
B$��B%��B&{B'
=B(Q�B)�B)��B*�HB,  B-�B-B.�HB0(�B1�B1�B3
=B4Q�B5p�B6{B733B8��B9p�B:ffB;�B<��B=��B>�\B?�
B@��BAG�BB�\BC�
BD��BEp�BF�\BG�
BI�BI�BJ�HBL  BMp�BN=qBO33BP��BQBR�\BS�BT��BV{BV�HBW�
BYG�BZffB[33B\(�B]p�B^�RB_�B`z�Ba�Bb�RBc�Bd��Bf=qBg33Bh  Bi�Bj�\Bk33Bl(�Bm��Bn�RBo�
Bp��Bq�Bs
=BtQ�Bu�Bv=qBw�By�Bz{B{33B|��B~{B
=B�{B��HB�p�B��B���B�\)B��B�ffB��B��
B�ffB��HB��B�Q�B��HB�\)B�(�B���B�G�B�  B��RB��B�B�z�B��HB�\)B��B��\B���B�\)B��
B�ffB��RB�
=B�p�B�  B�=qB��\B���B�G�B��B�{B�Q�B��\B��B��B��
B�  B�ffB���B�33B�p�B�  B�ffB��RB��HB�\)B��
B�  B�Q�B��RB�33B���B��
B�(�B��\B�
=B�\)B��B��B�ffB��RB��HB�33B�B�{B�Q�B��\B���B�p�B��B�  B�=qB��RB��B�p�B���B��B�ffB���B��B�\)B���B��B�z�B���B���B�33B��B�{B�ffB��\B���B�p�B��B��B�=qB���B�
=B�G�B���B�  B�ffB���B���B�G�B��B�(�B�z�B���B���B�G�B��B�(�B�z�B���B�
=B��B��
B�{B�ffB���B�33B���B��
B�{B�ffB��HB�33B�p�B�B�  B�z�B��HB�33B�\)B��B�  B�ffB��HB��B�\)B��B�{B�z�B���B�33B�p�B��
B�=qB��RB�
=B�G�B��B�  B�z�B��RB���B�G�B�B�=qB�z�B¸RB�33BÙ�B��B�(�B�z�B���B�\)Bř�B��B�Q�B���B���B�G�BǙ�B�  B�ffBȸRB���B�G�B�B�{B�ffBʣ�B��HB�\)B�B�(�B�Q�B̏\B�
=B�p�B��
B�(�B�Q�BθRB�33Bϙ�B��
B�{B�ffB���B�33BхB�B�  B�z�B���B�G�BӅB�B�(�BԸRB�
=B�G�BՅB�  B�ffB֣�B��HB�33Bי�B�{B�ffBأ�B��HB�33BٮB�  B�(�B�z�B��HB�G�Bۙ�B��
B�{B�z�B��HB�G�Bݙ�B��B�{B�ffB���B�G�Bߙ�B��B�(�B�ffB���B�G�B�B��B�=qB�\B���B�B��
B�(�B�z�B���B�p�B�B�  B�z�B��HB�G�B�B��
B�=qB�RB�33B�B�B�(�B��B��B�B��
B�{B�\B�
=B�B��
B�{B�\B��B�B�B�{B��B��B�p�B�B�=qB���B�
=B�\)B��
B�z�B��HB��B��B�(�B��RB���B�\)B��B�z�B��HB�G�B���B�{B���B�
=B�\)B��
B�ffB���B�33B��B�{B��RB��B�p�B��
C =qC z�C ��C �
C�CffC�C�RC  C=qCp�C��C��C{C\)C�\C�RC�C33Cz�C��C��C�C\)C�\C�C�C33CffC�C��C
=C=qCffC��C�C(�CQ�Cz�CC	
=C	33C	Q�C	�\C	�
C

=C
(�C
ffC
�C
�
C  CG�Cz�C��C�
C{C\)C�\C�RC�C�CffC�C�HC  C=qCz�CC�C{C\)C��C��C��C=qCz�C�RC�HC
=CG�C�\CC�C{C\)C��CC�C�C\)C��CC�C�CffC��C�RC�
C�CQ�Cz�C��C�
C{C=qC\)C�CC��C{C=qC�C�RC�HC
=C33CffC��C�HC{C33C\)C��C�
C
=C(�CQ�C�C��C  C{C\)C��C��C�C{CG�C�C�RC�HC  C(�CffC��C�RC�
C{CQ�Cz�C��C��C
=CG�Cz�C��C��C {C G�C p�C ��C �
C!{C!Q�C!�\C!�RC!�HC"{C"\)C"�\C"C"�HC#�C#ffC#��C#C#�HC$�C$\)C$��C$�RC$�HC%{C%\)C%�\C%�C%�HC&�C&\)C&�\C&�C&�
C'{C'Q�C'�C'�C'�
C(
=C(G�C(z�C(C)  C)�C)Q�C)�\C)�HC*{C*G�C*p�C*��C*�
C+�C+\)C+��C+�RC+�C,(�C,ffC,��C,��C,��C-33C-�C-�RC-�HC.
=C.G�C.�\C.��C.��C/(�C/Q�C/�\C/�
C0{C0=qC0ffC0�C0��C1(�C1Q�C1�C1�RC2
=C2=qC2ffC2��C2��C3�C3Q�C3�C3�RC3�C4�C4\)C4��C4�
C5
=C533C5ffC5��C5�HC6�C6\)C6�C6�RC6�C733C7ffC7�C7�C8�C8Q�C8p�C8�C9  C9=qC9z�C9��C9�
C:�C:ffC:��C:�HC;�C;G�C;p�C;�C;�C<=qC<z�C<��C<�
C=
=C=Q�C=�\C=��C>
=C>33C>ffC>��C>��C?33C?z�C?�C?�HC@{C@\)C@��C@�CA(�CA\)CA��CA��CB  CBG�CB��CB�
CC{CCG�CCz�CC��CD{CDQ�CD�\CDCD��CEG�CE�\CE�
CF�CF\)CF�CFCG
=CGQ�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                  ?u?��H@=p�@xQ�@�(�@��R@�G�A ��A  A\)A*=qA>�RA^�RA�  A�  A��A��A��A�  A�Q�A�B   B�
B�
B(�B (�B'�B/�
B7�
B?�
BG�
BP  BX  B`  Bh(�Bp  Bw�B�  B�{B��B�  B�  B�  B��B�{B�  B�  B�  B�  B�  B�  B�{B�{B�  B�{B�  B��B��B�{B�{B�{B�  B�  B�{B�(�B�{B�  B�  B�  C 
=C
=C
=C  C��C
  C
=C  C  C
=C
=C
=C��C  C{C  C   C"{C${C&{C(  C*
=C,{C.
=C/��C2  C4  C6  C8
=C:
=C<  C>  C?��CA��CD
=CF
=CH
=CJ
=CL  CN  CO��CR  CT
=CU�CX  CZ
=C\
=C^{C`�Cb
=Cd  Ce��Cg�Cj  Ck��Cn  Cp  Cr  Cs��Cv  Cw�Cy��C{��C~  C�  C���C�  C���C���C���C���C���C���C�  C�  C���C�C�  C���C���C�  C�C�C�
=C�C�  C�  C�C�  C�  C�  C�  C�  C���C���C�  C���C�  C�C�C�C�  C�  C�  C�C���C�  C�  C�
=C�C�
=C�  C�  C���C���C�C���C�C���C���C�  C�  C�C���C�C�  C�  C�  C�C���C�C�C�
=C�C���C�  C�  C���C�  C�C�  C�  C���C���C�  C�  C�C���C�  C�{C�C���C��C���C���C���C���C���C���C�  C���C���C�  C�  C���C���C�  C�
=C�C�C�C�  C�  C�C�C�  C�C�C���C�  C�  C�  C�  C�C�C���C�  C�C�C���C�  C�  D �D � D �qD� D  D}qD�qD}qD�D}qD�qD��D  D}qD�qD��D�qD� D	�D	��D	�qD
}qD
�qD� D�D}qD  D��D  D�D�D��D�D�D  Dz�D  D}qD  D� D  D}qD  D� D��D}qD�qD� D�qD� D�qD� D�qD� D  D� D��D}qD�qD}qD�qD� D  D� D   D ��D �qD!� D"  D"}qD#  D#}qD#��D$� D%  D%}qD&�D&� D'  D'� D'��D(z�D)  D)��D*�D*��D+  D+� D,�D,��D-D-��D-�qD.}qD.�qD/z�D/��D0}qD1  D1��D2D2�D3  D3}qD3�qD4z�D4�qD5}qD5��D6}qD6�qD7� D8�D8��D9D9��D:  D:}qD:�qD;z�D<  D<��D=�D=� D>D>�D?  D?� D?�qD@}qDADA��DB�DB� DC�DC�DD  DD� DE  DE��DF�DF�DG�DG}qDG��DH� DIDI��DJ  DJ� DK  DK}qDK�qDL� DL�qDM}qDN  DN��DO�DO�DP�DP� DP�qDQ� DR�DR� DS�DS� DS�qDT� DU  DU� DV  DV}qDV�qDWz�DW�qDX��DY�DY�DZ  DZ}qD[  D[� D[�qD\� D]�D]� D^  D^��D_�D_}qD_�qD`� D`�qDa� Db  Db}qDb�qDc� Dc�qDd}qDe�De� Df  Df��Dg�Dg� Dg�qDh}qDh�qDi}qDj  Dj� Dj�qDk� Dk��Dlz�Dl�qDmz�Dn  Dn��Dn�qDo� Dp�Dp��Dq  Dq� Dr�Dr�DsDs��Dt  Dt��Du�Du}qDv�Dv}qDw  Dw��Dx�Dx��Dx�qDy� Dz�Dz��D{�D{��D|�D|� D}�D}� D}�qD~�DD� D�qD�AHD��HD�� D���D�>�D�}qD�� D�HD�@ D�� D�� D�HD�B�D�� D���D���D�@ D��HD�� D���D�@ D�~�D���D�  D�AHD�� D�� D�  D�@ D��HD��HD�HD�AHD��HD���D���D�>�D�~�D�� D�HD�@ D�� D��HD���D�@ D��HD�� D�HD�@ D�� D��HD�  D�>�D�� D���D���D�AHD��HD��HD��D�AHD�� D��HD��D�AHD��HD�� D�  D�@ D��HD��HD���D�>�D�� D��HD�  D�=qD�}qD��qD��qD�=qD�}qD���D�  D�AHD�� D�� D�  D�@ D��HD��HD�HD�AHD���D��HD���D�@ D���D��HD�  D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�D���D��qD�>�D�� D��HD�HD�@ D�� D�� D��D�B�D�� D���D���D�AHD��HD�D��D�AHD�� D���D���D�>�D�� D�� D���D�>�D�� D��HD�HD�AHD�� D�� D���D�>�D�� D�� D�  D�@ D�� D��HD�  D�@ D�~�D�� D�HD�@ D�� D�� D�HD�@ D�� D���D�  D�AHD�~�D�� D���D�>�D�~�D�� D��D�B�D�� D�� D���D�>�D�~�D���D���D�@ D��HD�D�HD�AHD�� D�� D�HD�AHD��HD�D�HD�>�D�� D�� D�  D�AHD��HD���D�  D�@ D��HD���D�HD�>�D�}qD�� D�  D�>�D�� D�� D��qD�>�D�� D���D���D�@ D��HD�� D���D�AHD���D�� D�  D�AHD�~�D��HD�  D�=qD�~�D���D�  D�@ D���D�D�HD�AHD��HD�� D�HD�B�D��HD��HD�  D�@ D�� D�� D�  D�AHD��HD��qD���D�AHD��HD��HD�HD�@ D�� D�D��D�B�D���D�� D�HD�AHD D�� D�HD�AHD�}qD�� D��D�>�D�}qD�� D��D�AHD�~�Dž�D�  D�@ D�~�Dƾ�D�  D�AHDǀ D�� D��D�AHDȀ DȾ�D�HD�B�Dɀ D�� D���D�@ Dʀ D��HD�HD�AHDˁHD��HD�  D�>�D́HD�� D���D�>�D̀ D�D�  D�>�D�~�D��HD�HD�@ Dπ D�� D�  D�@ DЁHD�� D���D�=qD�}qDѾ�D�HD�AHD҂�D��HD�HD�@ D�~�D�� D�  D�AHDԁHD�� D���D�>�DՁHD�D�HD�AHDցHD��HD�  D�>�D�~�D��HD��D�AHD؁HD��HD�  D�@ Dـ D�� D�HD�@ D�~�D�� D��qD�@ DہHD۾�D�  D�AHD܁HD��HD�HD�@ D݀ D�� D���D�>�D�~�D�� D�HD�AHD߀ D�� D�HD�B�D���D�D��D�AHD� D��HD�  D�@ D� D��HD�HD�@ D� D�qD��qD�@ D� D�� D�  D�AHD�HD�� D�  D�>�D�HD�� D���D�@ D�HD�� D���D�>�D�~�D辸D�HD�AHD�}qD龸D�HD�@ D�HD��HD�  D�=qD�}qD�� D��D�@ D�~�D쾸D��qD�=qD�~�D�� D�  D�>�D�~�DD��D�B�D� D�� D�HD�AHD�� D�� D�  D�@ D�~�D�D�HD�B�D�HD�� D�  D�@ D� D��HD�  D�>�D�~�D���D���D�@ D���D��HD�  D�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D��HD��HD�  D�@ G�O�?�?W
=?�=q?���?�
=?��@\)@#�
@.{@E�@Y��@k�@z�H@��@�z�@��H@�ff@���@���@�G�@�{@�@�(�@�@�\)@�
=A�AA��A\)Az�AQ�A��A#33A&ffA*=qA0��A5�A7�A<��AB�\AG
=AJ=qAP��AU�AXQ�A^{Ac33AfffAj=qAp��As�
AxQ�A}p�A�  A��\A��A��RA���A��A��A��A��A��
A�A�Q�A��HA���A�
=A��A��A�p�A���A��HA�z�A�\)A�G�A��HA�p�A�  A���A�33A�{A��A���AÅA�A�
=A���A��
A�p�A�
=Aљ�A��
A�p�A�  A��AۅA�p�A�  AᙚA��HA��A�\)A�Q�A��A�(�A�A�
=A�G�A�33A�z�A�ffA���A�=qA��A�{A��B z�Bp�B�\B\)B  BG�B=qB�HB�B��B	B
ffB\)Bz�B��B{B
=B�BQ�Bp�BffB�HB�
B��Bp�B=qB\)BQ�B��B��B�HB�B  B�B�BffB�B Q�B ��B!��B"�\B#
=B#�
B$��B%��B&{B'
=B(Q�B)�B)��B*�HB,  B-�B-B.�HB0(�B1�B1�B3
=B4Q�B5p�B6{B733B8��B9p�B:ffB;�B<��B=��B>�\B?�
B@��BAG�BB�\BC�
BD��BEp�BF�\BG�
BI�BI�BJ�HBL  BMp�BN=qBO33BP��BQBR�\BS�BT��BV{BV�HBW�
BYG�BZffB[33B\(�B]p�B^�RB_�B`z�Ba�Bb�RBc�Bd��Bf=qBg33Bh  Bi�Bj�\Bk33Bl(�Bm��Bn�RBo�
Bp��Bq�Bs
=BtQ�Bu�Bv=qBw�By�Bz{B{33B|��B~{B
=B�{B��HB�p�B��B���B�\)B��B�ffB��B��
B�ffB��HB��B�Q�B��HB�\)B�(�B���B�G�B�  B��RB��B�B�z�B��HB�\)B��B��\B���B�\)B��
B�ffB��RB�
=B�p�B�  B�=qB��\B���B�G�B��B�{B�Q�B��\B��B��B��
B�  B�ffB���B�33B�p�B�  B�ffB��RB��HB�\)B��
B�  B�Q�B��RB�33B���B��
B�(�B��\B�
=B�\)B��B��B�ffB��RB��HB�33B�B�{B�Q�B��\B���B�p�B��B�  B�=qB��RB��B�p�B���B��B�ffB���B��B�\)B���B��B�z�B���B���B�33B��B�{B�ffB��\B���B�p�B��B��B�=qB���B�
=B�G�B���B�  B�ffB���B���B�G�B��B�(�B�z�B���B���B�G�B��B�(�B�z�B���B�
=B��B��
B�{B�ffB���B�33B���B��
B�{B�ffB��HB�33B�p�B�B�  B�z�B��HB�33B�\)B��B�  B�ffB��HB��B�\)B��B�{B�z�B���B�33B�p�B��
B�=qB��RB�
=B�G�B��B�  B�z�B��RB���B�G�B�B�=qB�z�B¸RB�33BÙ�B��B�(�B�z�B���B�\)Bř�B��B�Q�B���B���B�G�BǙ�B�  B�ffBȸRB���B�G�B�B�{B�ffBʣ�B��HB�\)B�B�(�B�Q�B̏\B�
=B�p�B��
B�(�B�Q�BθRB�33Bϙ�B��
B�{B�ffB���B�33BхB�B�  B�z�B���B�G�BӅB�B�(�BԸRB�
=B�G�BՅB�  B�ffB֣�B��HB�33Bי�B�{B�ffBأ�B��HB�33BٮB�  B�(�B�z�B��HB�G�Bۙ�B��
B�{B�z�B��HB�G�Bݙ�B��B�{B�ffB���B�G�Bߙ�B��B�(�B�ffB���B�G�B�B��B�=qB�\B���B�B��
B�(�B�z�B���B�p�B�B�  B�z�B��HB�G�B�B��
B�=qB�RB�33B�B�B�(�B��B��B�B��
B�{B�\B�
=B�B��
B�{B�\B��B�B�B�{B��B��B�p�B�B�=qB���B�
=B�\)B��
B�z�B��HB��B��B�(�B��RB���B�\)B��B�z�B��HB�G�B���B�{B���B�
=B�\)B��
B�ffB���B�33B��B�{B��RB��B�p�B��
C =qC z�C ��C �
C�CffC�C�RC  C=qCp�C��C��C{C\)C�\C�RC�C33Cz�C��C��C�C\)C�\C�C�C33CffC�C��C
=C=qCffC��C�C(�CQ�Cz�CC	
=C	33C	Q�C	�\C	�
C

=C
(�C
ffC
�C
�
C  CG�Cz�C��C�
C{C\)C�\C�RC�C�CffC�C�HC  C=qCz�CC�C{C\)C��C��C��C=qCz�C�RC�HC
=CG�C�\CC�C{C\)C��CC�C�C\)C��CC�C�CffC��C�RC�
C�CQ�Cz�C��C�
C{C=qC\)C�CC��C{C=qC�C�RC�HC
=C33CffC��C�HC{C33C\)C��C�
C
=C(�CQ�C�C��C  C{C\)C��C��C�C{CG�C�C�RC�HC  C(�CffC��C�RC�
C{CQ�Cz�C��C��C
=CG�Cz�C��C��C {C G�C p�C ��C �
C!{C!Q�C!�\C!�RC!�HC"{C"\)C"�\C"C"�HC#�C#ffC#��C#C#�HC$�C$\)C$��C$�RC$�HC%{C%\)C%�\C%�C%�HC&�C&\)C&�\C&�C&�
C'{C'Q�C'�C'�C'�
C(
=C(G�C(z�C(C)  C)�C)Q�C)�\C)�HC*{C*G�C*p�C*��C*�
C+�C+\)C+��C+�RC+�C,(�C,ffC,��C,��C,��C-33C-�C-�RC-�HC.
=C.G�C.�\C.��C.��C/(�C/Q�C/�\C/�
C0{C0=qC0ffC0�C0��C1(�C1Q�C1�C1�RC2
=C2=qC2ffC2��C2��C3�C3Q�C3�C3�RC3�C4�C4\)C4��C4�
C5
=C533C5ffC5��C5�HC6�C6\)C6�C6�RC6�C733C7ffC7�C7�C8�C8Q�C8p�C8�C9  C9=qC9z�C9��C9�
C:�C:ffC:��C:�HC;�C;G�C;p�C;�C;�C<=qC<z�C<��C<�
C=
=C=Q�C=�\C=��C>
=C>33C>ffC>��C>��C?33C?z�C?�C?�HC@{C@\)C@��C@�CA(�CA\)CA��CA��CB  CBG�CB��CB�
CC{CCG�CCz�CC��CD{CDQ�CD�\CDCD��CEG�CE�\CE�
CF�CF\)CF�CFCG
=CGQ�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��HA��HA��TA��`A��TA��`A��`A��`A��`A��TA��mA��mA��mA��mA��yA��yA��yA��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A޺^A�bNA�|�A���Aا�A��A�I�A�A�hsA�ƨA�  A�XA��;A��A���A�/A�K�A�33Aɝ�A�"�A�n�A�oA�"�A�/AöFA�+A�z�A���A�A��A�1A�XA�;dA�x�A��A�I�A��A���A�n�A���A�r�A�G�A�/A��A���A���A���A�bA��A��jA�x�A�A���A�A��A��A�+A��FA��A�~�A��A���A��A��A���A��RA���A��uA��9A�
=A�z�A�n�A���A�33A�9XA��wA�r�A�A��A�+A�(�A�G�A��A�9XA~Q�A{�Ax��AvĜAu�AsS�Aq/AmC�AiƨAg�Af�+AdffAc;dAbz�AaO�A_�FA^�9A];dAY�AYx�AX�/AX�uAXI�AWdZAV�ATjASVAR �AQp�AO\)AM�FAMXALI�AI|�AG��AG7LAE\)A>A�A9�
A7��A7�PA7O�A77LA7�A6��A5
=A3�;A1��A.��A-�A+��A)�^A(^5A(�A(JA'��A'A&�`A&jA%&�A$�9A$�DA$5?A#��A#�TA#�
A#��A#/A"�yA"�DA!�A �yA n�A|�A�jA�AbNA9XA{A�A�A�A+A�`A~�A$�A�
A�/A+A��AO�A\)A �A��AdZA�9AbNA{A�^AoA
ZA�A�AO�A�wA�A�A�RAVA��A��AS�A ��A 1'@��R@���@���@�G�@�Q�@���@�ƨ@�t�@��@�J@�/@�;d@�x�@��j@�C�@���@�+@�$�@�9X@��m@�S�@�S�@@�@�ƨ@���@��;@���@�~�@�9X@�@�!@�  @�h@��;@�w@�Q�@�O�@�ff@�h@�j@�
=@�x�@�|�@�7L@�|�@��@��@ܬ@ە�@���@�I�@�l�@ׅ@���@��@���@Չ7@�\)@��@�`B@�Ĝ@�(�@υ@��y@�^5@��#@�x�@��@�z�@�Q�@�A�@�ƨ@�"�@ʗ�@�hs@�hs@�/@ȃ@�
=@ēu@��;@�|�@�+@�"�@�+@�"�@�\)@�dZ@��m@�9X@�A�@���@�v�@���@��@�z�@��u@��j@�&�@�z�@��@��F@���@���@��@�t�@�l�@�S�@�
=@�5?@�%@���@��P@�33@���@�5?@���@��-@���@��7@�/@�/@�G�@�O�@��^@��T@��@�x�@�r�@�r�@��@�C�@�33@��@�v�@�@�@���@��h@�p�@�G�@���@�ƨ@���@�V@�E�@���@�X@�V@��/@��u@���@�|�@�ff@��T@���@���@��^@���@�p�@�?}@�%@��@�b@���@��w@�\)@���@��y@�^5@���@��h@�7L@�/@��@��j@�Z@�1'@��P@�\)@�;d@�@�
=@�@���@���@�v�@�V@�=q@�=q@�5?@�$�@�J@�@�@�@��T@��-@�`B@��/@�r�@�9X@�(�@� �@�ƨ@�+@�^5@���@�?}@�Ĝ@�(�@���@��y@�M�@��#@�%@� �@���@�ƨ@���@�33@�ȴ@��@�J@���@�@��T@���@���@�?}@�9X@�dZ@�K�@��@���@��R@�{@���@���@�j@�I�@�1@���@��m@���@��w@��F@��@��@�dZ@�C�@��!@�E�@�$�@��7@�/@���@�9X@�1'@���@��F@���@��P@��@�|�@�\)@�;d@�o@��y@���@���@��+@��+@�~�@�~�@�~�@�~�@�ff@�M�@���@�&�@���@�I�@��@��@�ƨ@��w@��F@��F@��w@��w@��@�
=@���@��\@�^5@�$�@���@�@��7@�hs@�/@���@��u@�j@�bN@�I�@��@��m@���@���@���@���@���@���@��P@��@��@�t�@�\)@�S�@��@��H@���@���@���@�/@��@��/@�j@�1@��;@��;@��
@���@���@�S�@�
=@�@��H@��!@���@��+@�v�@�M�@�{@��@��-@��7@�`B@�?}@�%@��@�Ĝ@���@�r�@�Q�@�A�@�1'@�1@��@|�@;d@~ȴ@~5?@}��@|�@|��@|j@{��@{��@{t�@{"�@z�!@z��@z��@z~�@zJ@yG�@xĜ@xbN@x1'@x  @w�;@w|�@w�@vff@v@t�/@s33@s@r�@r�@r�H@r��@r��@r�\@rn�@rJ@q��@p�9@ol�@n�@nff@n@m@m�h@mp�@m`B@m/@l��@lz�@l�@k��@j�@j^5@i��@iX@h��@hr�@h �@g��@f��@fv�@f@e��@ep�@ep�@ep�@eO�@e/@e/@e?}@e/@d��@dZ@cS�@b~�@a��@`��@`�9@`bN@_�;@_
=@^E�@]��@]�-@]�-@]�-@]�-@]�-@]�-@]�-@]�-@]�-@]�h@]O�@\��@[�
@[@Z�\@Z=q@Z�@Y��@Y�@Y�7@X��@XĜ@X��@Xr�@XA�@XA�@W�@Vv�@V@U�-@U�h@Up�@U`B@U�@T�j@Tz�@T�@S��@S��@R��@R^5@Q��@Q&�@P�9@Pr�@P  @O��@O+@Nȴ@N{@M@M@M��@MO�@L��@L��@LZ@LI�@L�@KdZ@J��@J-@I�^@I��@Ihs@IX@I&�@H��@H��@H�u@HbN@HQ�@H �@G�@G��@G|�@F�@D�@C��@C�@CS�@CC�@C33@B�@BM�@Ahs@A7L@A%@@�@@A�@?��@?+@?
=@>�@>V@=@=�-@=`B@=�@=V@<�@<�@<�D@<z�@<Z@;�@;@;@;@;@:��@:�\@:M�@:�@9��@8�9@8bN@8  @7�@7l�@5�T@4�@4��@4�D@4j@3�m@3�@333@2~�@1��@1��@1�7@1x�@1hs@1G�@0Ĝ@0��@0bN@/�;@/l�@/
=@.��@.v�@.5?@.{@.@.@-��@-p�@-�@-p�@-O�@-/@,�/@,��@,I�@,1@+ƨ@+33@*��@*��@*~�@*n�@*�@*J@*J@)G�@(�u@(bN@(  @(  @'�@'�w@'��@';d@&V@%�@%�-@$��@$z�@$j@$Z@$I�@#�m@#33@#"�@"��@"�@!�^@!7L@ 1'@��@;d@�y@�@�R@�+@E�@�@�h@�h@O�@Z@1@��@�@S�@��@��@�@��@�^@��@��@��@��@x�@hs@��@�`@��@��@�9@��@r�@A�@b@�w@ȴ@$�@�@��@@��@�T@�@�@�@�T@��@��@�h@�h@�@p�@O�@/@V@V@V@��@V@V@��@��@�j@�@�@��@j@9X@�m@�
@ƨ@�F@�@33@��@^5@=q@��@�7@hs@X@X@G�@�`@Q�@A�@1'@b@��@|�@K�@�@�@�y@�R@��@��@��@��@�+@ff@E�@{@@�@�T@��@O�@Z@��@��@dZ@C�@@
�@
�@
�@
�@
�@
�@
�@
�@
�@
�@
�@
�@
��@
��@
��@
�\A��HA��mA��HA��HA��mA��;A��;A��TA��;A��HA��`A��TA��HA��mA��`A��HA��`A��mA��TA��HA��mA��TA��HA��`A��`A��HA��mA��`A��HA��mA��mA��TA��TA��mA��`A��TA��mA��yA��TA��`A��yA��`A��TA��yA��mA��TA��mA��yA��`A��`A��yA��TA��mA��yA��`A��yA��A��`A��mA��yA��`A��mA��A��mA��`A��yA��A��`A��mA��A��mA��`A��A��mA��`A��A��yA��`A��yA��A��yA��mA��A��yA��mA��A��A��yA��yA��A��yA��mA��A��A��yA��A��A��yA��yA��A��A��yA��A��A��yA��A��A��A��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A���A���A��A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A�  A���A��A�Aޣ�A�v�A�5?A���AݼjA�dZA�M�A�=qA�A���AܮAܡ�A܏\A�|�A�\)A�A���Aۡ�A�XA�K�A���Aٗ�A�v�A�S�A��Aأ�A�O�A�VA��Aׇ+A�?}A�"�A���A��AָRA�Q�A��A���A��AվwAթ�A�v�A�A�A�  A��AԴ9A�n�A�;dA��A�Aӛ�A�dZA�E�A��#A�5?A��HAхA�^5A�JA��A�ȴAБhA�x�A�n�A�\)A�E�A�5?A�+A�"�A��A�oA�VA��A��HA���A�A�ȴA�ƨAϬA�~�A�t�A�n�A�ffA�^5A�^5A�ZA�Q�A�$�A���A���AΣ�A�~�A�\)A�E�A�E�A�9XA�7LA�A�A�I�A�VA�v�A�ffA�ZA�hsAΉ7AΏ\A΍PA�`BA�I�A�1'A�{A��A���Aͺ^A͙�A�hsA�  A̴9A�p�A�C�A�9XA�33A�$�A˲-A�z�A�n�A�hsA�n�A�r�A�~�AˍPA˛�A˲-A�ĜA��HA��A�1A��A�?}A�t�ÃA̕�A�r�A�bNA�\)A�I�A�=qA�33A�&�A��A���A��HA��`A��`A���A˾wAˡ�AˁA�z�A�r�A�M�A��TAʾwAʴ9Aʣ�AʍPA�p�A�dZA�I�A�-A�$�A�"�A��A�A���A���A��;A��
A�ȴAɮAɣ�Aɕ�AɍPAɃAɃA�v�A�|�A�p�A�dZA�VA�&�A��A��A��A�oA�1A�%A�
=A�oA�oA�bA�1A���A��/AȸRAȡ�A�VA�/AǗ�A�VA�33A���A�ƨAƍPA�l�A��A�  A���AżjA�t�A�|�Aŕ�AőhAŉ7AőhA�|�A�`BA�XA�A�A�"�A�oA��AĸRAğ�A�v�A�bNA�VA�E�A�-A�&�A�(�A�&�A� �A�{A��A�{A�%A�%A���A���A���A���AìAÕ�A�x�AÁAÉ7AÉ7AÑhA�E�A���A���A���A��A��A°!A�VA���A���A�z�A�A�A��A�bNA���A���A�M�A��A�^5A�5?A�VA��A��mA��A���A���A��wA��uA��A��A��A��A�v�A�ffA�G�A��A�bA��A�ƨA��A��hA�S�A��A��A�M�A�(�A��A�VA�1A�
=A�A���A���A��A���A�l�A�&�A���A��#A���A���A�I�A���A��;A���A�A��+A��A��yA��;A�ĜA���A�~�A�`BA�VA�A�A�-A��A���A��A��
A��RA���A�  A��A��HA���A�z�A�O�A��A�A���A��A��A���A��jA��A���A���A��DA�z�A�ffA�G�A�7LA�&�A�JA�%A�A�A�A�A�  A��A��/A���A���A���A��hA�`BA�/A��#A���A�bNA�?}A�oA��A���A��^A��A��uA�r�A�M�A�7LA� �A�1A���A�ƨA���A�=qA�  A���A���A�C�A��yA��jA��+A��A�z�A�v�A�n�A�jA�hsA�dZA�S�A�O�A�M�A�K�A�I�A�G�A�E�A�C�A�?}A�9XA�5?A�7LA�1'A�/A�-A�(�A�+A�-A�+A�&�A� �A��A��A��A��A��A��A��A�{A�{A��A�VA�VA�
=A�
=A�1A�%A���A��HA���A���A�ĜA��wA��FA���A��uA�dZA���A�VA�bA�A���A��A��TA���A��FA��A��A�r�A�l�A�ffA�^5A�p�A�I�A��A�
=A���A�  A���A��yA��A��A�  A���A���A��\A���A���A���A�z�A�p�A�\)A��A�{A�{A�ƨA�ȴA���A���A���A��-A���A���A��\A���A���A���A�~�A�VA�n�A�`BA�bNA�S�A�Q�A�?}A�oA�A�1A�A��A��A��#A��HA���A��^A��FA��-A��A��hA��DA��7A��DA��DA��A�~�A�l�A�S�A�$�A�$�A�A�A�O�A���A�oA�  A��A��TA�ƨA���A���A��+A�r�A�l�A�hsA�S�A�?}A�1'A�(�A�%A��#A���A�x�A�S�A�C�A�;dA�1'A�"�A� �A��A�JA�  A��A��;A���A��jA��FA���A��7A��+A�x�A�l�A�XA�?}A�33A�7LA�-A��/A��!A���A���A���A���A��hA�|�A�r�A�r�A�^5A�bNA�dZA�Q�A�I�A�?}A�33A�oA�JA���A��TA��wA���A�XA�(�A��A��RA��DA�dZA�M�A���A��A��-A�r�A��A�ffA��A�ȴA�VA��A��7A�p�A�jA�`BA�;dA��A�bNA�(�A�A��A���A���A�x�A�r�A�jA�XA�33A��A��A��^A�7LA��A��hA�5?A��A�bA�A���A��A��A��A��A��A��`A��#A���A��wA���A�|�A�^5A�;dA��A���A��/A�A��-A���A��DA�x�A�bNA�M�A�7LA�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                  A��TA��HA��HA��TA��`A��TA��`A��`A��`A��`A��TA��mA��mA��mA��mA��yA��yA��yA��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A޺^A�bNA�|�A���Aا�A��A�I�A�A�hsA�ƨA�  A�XA��;A��A���A�/A�K�A�33Aɝ�A�"�A�n�A�oA�"�A�/AöFA�+A�z�A���A�A��A�1A�XA�;dA�x�A��A�I�A��A���A�n�A���A�r�A�G�A�/A��A���A���A���A�bA��A��jA�x�A�A���A�A��A��A�+A��FA��A�~�A��A���A��A��A���A��RA���A��uA��9A�
=A�z�A�n�A���A�33A�9XA��wA�r�A�A��A�+A�(�A�G�A��A�9XA~Q�A{�Ax��AvĜAu�AsS�Aq/AmC�AiƨAg�Af�+AdffAc;dAbz�AaO�A_�FA^�9A];dAY�AYx�AX�/AX�uAXI�AWdZAV�ATjASVAR �AQp�AO\)AM�FAMXALI�AI|�AG��AG7LAE\)A>A�A9�
A7��A7�PA7O�A77LA7�A6��A5
=A3�;A1��A.��A-�A+��A)�^A(^5A(�A(JA'��A'A&�`A&jA%&�A$�9A$�DA$5?A#��A#�TA#�
A#��A#/A"�yA"�DA!�A �yA n�A|�A�jA�AbNA9XA{A�A�A�A+A�`A~�A$�A�
A�/A+A��AO�A\)A �A��AdZA�9AbNA{A�^AoA
ZA�A�AO�A�wA�A�A�RAVA��A��AS�A ��A 1'@��R@���@���@�G�@�Q�@���@�ƨ@�t�@��@�J@�/@�;d@�x�@��j@�C�@���@�+@�$�@�9X@��m@�S�@�S�@@�@�ƨ@���@��;@���@�~�@�9X@�@�!@�  @�h@��;@�w@�Q�@�O�@�ff@�h@�j@�
=@�x�@�|�@�7L@�|�@��@��@ܬ@ە�@���@�I�@�l�@ׅ@���@��@���@Չ7@�\)@��@�`B@�Ĝ@�(�@υ@��y@�^5@��#@�x�@��@�z�@�Q�@�A�@�ƨ@�"�@ʗ�@�hs@�hs@�/@ȃ@�
=@ēu@��;@�|�@�+@�"�@�+@�"�@�\)@�dZ@��m@�9X@�A�@���@�v�@���@��@�z�@��u@��j@�&�@�z�@��@��F@���@���@��@�t�@�l�@�S�@�
=@�5?@�%@���@��P@�33@���@�5?@���@��-@���@��7@�/@�/@�G�@�O�@��^@��T@��@�x�@�r�@�r�@��@�C�@�33@��@�v�@�@�@���@��h@�p�@�G�@���@�ƨ@���@�V@�E�@���@�X@�V@��/@��u@���@�|�@�ff@��T@���@���@��^@���@�p�@�?}@�%@��@�b@���@��w@�\)@���@��y@�^5@���@��h@�7L@�/@��@��j@�Z@�1'@��P@�\)@�;d@�@�
=@�@���@���@�v�@�V@�=q@�=q@�5?@�$�@�J@�@�@�@��T@��-@�`B@��/@�r�@�9X@�(�@� �@�ƨ@�+@�^5@���@�?}@�Ĝ@�(�@���@��y@�M�@��#@�%@� �@���@�ƨ@���@�33@�ȴ@��@�J@���@�@��T@���@���@�?}@�9X@�dZ@�K�@��@���@��R@�{@���@���@�j@�I�@�1@���@��m@���@��w@��F@��@��@�dZ@�C�@��!@�E�@�$�@��7@�/@���@�9X@�1'@���@��F@���@��P@��@�|�@�\)@�;d@�o@��y@���@���@��+@��+@�~�@�~�@�~�@�~�@�ff@�M�@���@�&�@���@�I�@��@��@�ƨ@��w@��F@��F@��w@��w@��@�
=@���@��\@�^5@�$�@���@�@��7@�hs@�/@���@��u@�j@�bN@�I�@��@��m@���@���@���@���@���@���@��P@��@��@�t�@�\)@�S�@��@��H@���@���@���@�/@��@��/@�j@�1@��;@��;@��
@���@���@�S�@�
=@�@��H@��!@���@��+@�v�@�M�@�{@��@��-@��7@�`B@�?}@�%@��@�Ĝ@���@�r�@�Q�@�A�@�1'@�1@��@|�@;d@~ȴ@~5?@}��@|�@|��@|j@{��@{��@{t�@{"�@z�!@z��@z��@z~�@zJ@yG�@xĜ@xbN@x1'@x  @w�;@w|�@w�@vff@v@t�/@s33@s@r�@r�@r�H@r��@r��@r�\@rn�@rJ@q��@p�9@ol�@n�@nff@n@m@m�h@mp�@m`B@m/@l��@lz�@l�@k��@j�@j^5@i��@iX@h��@hr�@h �@g��@f��@fv�@f@e��@ep�@ep�@ep�@eO�@e/@e/@e?}@e/@d��@dZ@cS�@b~�@a��@`��@`�9@`bN@_�;@_
=@^E�@]��@]�-@]�-@]�-@]�-@]�-@]�-@]�-@]�-@]�-@]�h@]O�@\��@[�
@[@Z�\@Z=q@Z�@Y��@Y�@Y�7@X��@XĜ@X��@Xr�@XA�@XA�@W�@Vv�@V@U�-@U�h@Up�@U`B@U�@T�j@Tz�@T�@S��@S��@R��@R^5@Q��@Q&�@P�9@Pr�@P  @O��@O+@Nȴ@N{@M@M@M��@MO�@L��@L��@LZ@LI�@L�@KdZ@J��@J-@I�^@I��@Ihs@IX@I&�@H��@H��@H�u@HbN@HQ�@H �@G�@G��@G|�@F�@D�@C��@C�@CS�@CC�@C33@B�@BM�@Ahs@A7L@A%@@�@@A�@?��@?+@?
=@>�@>V@=@=�-@=`B@=�@=V@<�@<�@<�D@<z�@<Z@;�@;@;@;@;@:��@:�\@:M�@:�@9��@8�9@8bN@8  @7�@7l�@5�T@4�@4��@4�D@4j@3�m@3�@333@2~�@1��@1��@1�7@1x�@1hs@1G�@0Ĝ@0��@0bN@/�;@/l�@/
=@.��@.v�@.5?@.{@.@.@-��@-p�@-�@-p�@-O�@-/@,�/@,��@,I�@,1@+ƨ@+33@*��@*��@*~�@*n�@*�@*J@*J@)G�@(�u@(bN@(  @(  @'�@'�w@'��@';d@&V@%�@%�-@$��@$z�@$j@$Z@$I�@#�m@#33@#"�@"��@"�@!�^@!7L@ 1'@��@;d@�y@�@�R@�+@E�@�@�h@�h@O�@Z@1@��@�@S�@��@��@�@��@�^@��@��@��@��@x�@hs@��@�`@��@��@�9@��@r�@A�@b@�w@ȴ@$�@�@��@@��@�T@�@�@�@�T@��@��@�h@�h@�@p�@O�@/@V@V@V@��@V@V@��@��@�j@�@�@��@j@9X@�m@�
@ƨ@�F@�@33@��@^5@=q@��@�7@hs@X@X@G�@�`@Q�@A�@1'@b@��@|�@K�@�@�@�y@�R@��@��@��@��@�+@ff@E�@{@@�@�T@��@O�@Z@��@��@dZ@C�@@
�@
�@
�@
�@
�@
�@
�@
�@
�@
�@
�@
�@
��@
��@
��G�O�A��HA��mA��HA��HA��mA��;A��;A��TA��;A��HA��`A��TA��HA��mA��`A��HA��`A��mA��TA��HA��mA��TA��HA��`A��`A��HA��mA��`A��HA��mA��mA��TA��TA��mA��`A��TA��mA��yA��TA��`A��yA��`A��TA��yA��mA��TA��mA��yA��`A��`A��yA��TA��mA��yA��`A��yA��A��`A��mA��yA��`A��mA��A��mA��`A��yA��A��`A��mA��A��mA��`A��A��mA��`A��A��yA��`A��yA��A��yA��mA��A��yA��mA��A��A��yA��yA��A��yA��mA��A��A��yA��A��A��yA��yA��A��A��yA��A��A��yA��A��A��A��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A���A���A��A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A�  A���A��A�Aޣ�A�v�A�5?A���AݼjA�dZA�M�A�=qA�A���AܮAܡ�A܏\A�|�A�\)A�A���Aۡ�A�XA�K�A���Aٗ�A�v�A�S�A��Aأ�A�O�A�VA��Aׇ+A�?}A�"�A���A��AָRA�Q�A��A���A��AվwAթ�A�v�A�A�A�  A��AԴ9A�n�A�;dA��A�Aӛ�A�dZA�E�A��#A�5?A��HAхA�^5A�JA��A�ȴAБhA�x�A�n�A�\)A�E�A�5?A�+A�"�A��A�oA�VA��A��HA���A�A�ȴA�ƨAϬA�~�A�t�A�n�A�ffA�^5A�^5A�ZA�Q�A�$�A���A���AΣ�A�~�A�\)A�E�A�E�A�9XA�7LA�A�A�I�A�VA�v�A�ffA�ZA�hsAΉ7AΏ\A΍PA�`BA�I�A�1'A�{A��A���Aͺ^A͙�A�hsA�  A̴9A�p�A�C�A�9XA�33A�$�A˲-A�z�A�n�A�hsA�n�A�r�A�~�AˍPA˛�A˲-A�ĜA��HA��A�1A��A�?}A�t�ÃA̕�A�r�A�bNA�\)A�I�A�=qA�33A�&�A��A���A��HA��`A��`A���A˾wAˡ�AˁA�z�A�r�A�M�A��TAʾwAʴ9Aʣ�AʍPA�p�A�dZA�I�A�-A�$�A�"�A��A�A���A���A��;A��
A�ȴAɮAɣ�Aɕ�AɍPAɃAɃA�v�A�|�A�p�A�dZA�VA�&�A��A��A��A�oA�1A�%A�
=A�oA�oA�bA�1A���A��/AȸRAȡ�A�VA�/AǗ�A�VA�33A���A�ƨAƍPA�l�A��A�  A���AżjA�t�A�|�Aŕ�AőhAŉ7AőhA�|�A�`BA�XA�A�A�"�A�oA��AĸRAğ�A�v�A�bNA�VA�E�A�-A�&�A�(�A�&�A� �A�{A��A�{A�%A�%A���A���A���A���AìAÕ�A�x�AÁAÉ7AÉ7AÑhA�E�A���A���A���A��A��A°!A�VA���A���A�z�A�A�A��A�bNA���A���A�M�A��A�^5A�5?A�VA��A��mA��A���A���A��wA��uA��A��A��A��A�v�A�ffA�G�A��A�bA��A�ƨA��A��hA�S�A��A��A�M�A�(�A��A�VA�1A�
=A�A���A���A��A���A�l�A�&�A���A��#A���A���A�I�A���A��;A���A�A��+A��A��yA��;A�ĜA���A�~�A�`BA�VA�A�A�-A��A���A��A��
A��RA���A�  A��A��HA���A�z�A�O�A��A�A���A��A��A���A��jA��A���A���A��DA�z�A�ffA�G�A�7LA�&�A�JA�%A�A�A�A�A�  A��A��/A���A���A���A��hA�`BA�/A��#A���A�bNA�?}A�oA��A���A��^A��A��uA�r�A�M�A�7LA� �A�1A���A�ƨA���A�=qA�  A���A���A�C�A��yA��jA��+A��A�z�A�v�A�n�A�jA�hsA�dZA�S�A�O�A�M�A�K�A�I�A�G�A�E�A�C�A�?}A�9XA�5?A�7LA�1'A�/A�-A�(�A�+A�-A�+A�&�A� �A��A��A��A��A��A��A��A�{A�{A��A�VA�VA�
=A�
=A�1A�%A���A��HA���A���A�ĜA��wA��FA���A��uA�dZA���A�VA�bA�A���A��A��TA���A��FA��A��A�r�A�l�A�ffA�^5A�p�A�I�A��A�
=A���A�  A���A��yA��A��A�  A���A���A��\A���A���A���A�z�A�p�A�\)A��A�{A�{A�ƨA�ȴA���A���A���A��-A���A���A��\A���A���A���A�~�A�VA�n�A�`BA�bNA�S�A�Q�A�?}A�oA�A�1A�A��A��A��#A��HA���A��^A��FA��-A��A��hA��DA��7A��DA��DA��A�~�A�l�A�S�A�$�A�$�A�A�A�O�A���A�oA�  A��A��TA�ƨA���A���A��+A�r�A�l�A�hsA�S�A�?}A�1'A�(�A�%A��#A���A�x�A�S�A�C�A�;dA�1'A�"�A� �A��A�JA�  A��A��;A���A��jA��FA���A��7A��+A�x�A�l�A�XA�?}A�33A�7LA�-A��/A��!A���A���A���A���A��hA�|�A�r�A�r�A�^5A�bNA�dZA�Q�A�I�A�?}A�33A�oA�JA���A��TA��wA���A�XA�(�A��A��RA��DA�dZA�M�A���A��A��-A�r�A��A�ffA��A�ȴA�VA��A��7A�p�A�jA�`BA�;dA��A�bNA�(�A�A��A���A���A�x�A�r�A�jA�XA�33A��A��A��^A�7LA��A��hA�5?A��A�bA�A���A��A��A��A��A��A��`A��#A���A��wA���A�|�A�^5A�;dA��A���A��/A�A��-A���A��DA�x�A�bNA�M�A�7LA�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BuZBt�Bt�BtTBt�Bt�Bt�Bt�Bs�Bs�Bt�Bs�Bs�Bs�Bs�BtBtTBtBtTBtBt�Bt�Bt�Bt�Bu%BuZBuZBu%Bu%Bt�Bt�BtTBtBs�BsBq�B��B�BE�B��B��B��Bl�Bv�BUgBC�B4nB?�B^5BB[B�1B�?B�`B�8B�`B�+B��B�RB�B��B�
B�B}"BaBaHBXyBW?BLdBEmBJ�BH�BR�BW�BL0B>BC-BDgBD3BE9BE�BEBOBBMjBL�BNpBM�BM�BMjBL0BM6BA�B3�B-wB*�B'�B�B�B�B��B�HBʌB��B�MB��B�MB}�BpoB_pBR�BPB5�B)*B%zB�B.BoB
�B
��B
��B
��B
��B
��B
��B
|�B
o5B
g8B
T�B
IB
-�B
%zB
B
�B
	lB
SB	��B	�B	�/B	��B	ٴB	��B	҉B	�vB	�jB	�)B	ÖB	�qB	��B	��B	��B	�FB	��B	�uB	�(B	��B	v�B	qAB	j�B	Z�B	I�B	<B	<6B	9�B	9�B	9XB	7�B	5B	2�B	1�B	*�B	(�B	*eB	*�B	(�B	'�B	'�B	&�B	&�B	(�B	&�B	*�B	*0B	+6B	-B	.IB	-�B	.B	-�B	/�B	/B	/�B	2�B	4B	7B	;0B	;�B	;0B	9�B	8B	8�B	>BB	>�B	=<B	>�B	>�B	>wB	=�B	<6B	>�B	B�B	<�B	J�B	;�B	6B	3�B	2�B	2�B	2aB	/OB	/�B	/�B	-�B	4B	3�B	2�B	5B	2�B	6�B	5?B	3�B	3�B	1�B	2aB	3�B	6FB	7B	9�B	9�B	:�B	=<B	?�B	@B	@OB	AUB	@B	=<B	?B	?}B	?�B	?�B	?HB	>wB	>wB	@B	B'B	GB	K�B	O�B	QNB	W�B	aB	h>B	l�B	tTB	m)B	l"B	k�B	j�B	jB	m�B	m)B	�PB	�\B	��B	�tB	��B	�B	��B	�UB	�=B	�B	��B	�bB	�~B	��B	��B	��B	�B	��B	�*B	��B	�LB	��B	��B	��B	��B	�HB	��B	�KB	�B	�B	�&B	�aB	�gB	�B	��B	��B	��B	چB	یB	�KB	چB	�/B	��B	یB	��B	��B	ҽB	ԕB	�2B	�B	֡B	�yB	چB	�]B	�>B	�B	�)B	�B	�B	�B	��B	�B	�B	�B	��B	�B	��B	��B	��B	�B	�B	�B	�TB	��B	��B	�VB	��B
B
 4B
�B
�B
GB
uB
B
�B
�B
B
�B
YB
	lB
�B
bB
MB
�B
�B
B
�B
�B
�B
%FB
%zB
%�B
%zB
%B
%B
$�B
'RB
)*B
,qB
+�B
+�B
-�B
.�B
.}B
.�B
/�B
1�B
1'B
5�B
6FB
6�B
6�B
6�B
6�B
7B
7LB
7�B
8�B
:^B
9�B
9�B
;0B
:�B
:�B
<jB
<�B
=qB
=�B
=�B
=�B
=�B
>B
>BB
@B
?�B
@�B
AUB
AUB
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B[B
B[B
B'B
B�B
B[B
A�B
B�B
C-B
DgB
EB
E�B
E�B
E�B
F�B
GB
G�B
G�B
F�B
EB
C�B
D�B
GEB
F�B
G�B
H�B
J�B
L�B
N�B
OvB
O�B
O�B
PHB
P�B
Q�B
R�B
V9B
V9B
U�B
UgB
UgB
T�B
U2B
T�B
U2B
U�B
V9B
W
B
W?B
WsB
XB
YB
Y�B
ZB
ZQB
[WB
[WB
Y�B
W�B
WsB
X�B
YKB
\]B
^�B
_B
_�B
`vB
aHB
a|B
a�B
a�B
bB
bB
a�B
bNB
bNB
b�B
b�B
b�B
b�B
b�B
c B
b�B
cTB
b�B
c�B
a�B
c B
a�B
bNB
b�B
b�B
c B
c B
cTB
cTB
cTB
c�B
h
B
h>B
h�B
h�B
iB
iDB
i�B
jB
jB
jB
j�B
k�B
l"B
l"B
l"B
l�B
l�B
l�B
m]B
m)B
m)B
m)B
m)B
m]B
m]B
m)B
m)B
m]B
m)B
m�B
m�B
m�B
m�B
o�B
o�B
o�B
p;B
p�B
qvB
qAB
qAB
qAB
qAB
qvB
q�B
rB
q�B
rB
rB
r�B
r|B
r|B
r�B
sMB
r�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
uZB
uZB
uZB
uZB
u�B
u�B
u�B
u�B
v+B
v�B
v�B
w�B
w�B
xB
xB
x8B
xB
x�B
x�B
x�B
x�B
x8B
x�B
y>B
y�B
y�B
y�B
yrB
y�B
y�B
zB
zDB
zB
|B
|�B
|�B
|�B
|�B
|�B
}"B
}"B
|�B
}"B
}�B
}�B
.B
�B
�B
� B
�4B
�4B
�4B
�4B
�4B
�4B
��B
��B
�B
�oB
�uB
�B
�GB
�GB
��B
�B
�B
�B
��B
��B
�SB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�YB
�_B
�_B
�1B
��B
��B
��B
�7B
�	B
��B
�B
�B
��B
�DB
��B
�B
��B
��B
��B
��B
��B
��B
�DB
�JB
�~B
�PB
�B
��B
�B
��B
��B
��B
��B
��B
�VB
�VB
�"B
��B
��B
��B
�bB
�.B
�.B
�bB
��B
��B
� B
�4B
� B
�hB
�:B
�:B
��B
�B
�B
�@B
�B
�B
�B
�uB
�B
�B
�B
��B
��B
��B
�MB
�B
�B
�B
�B
�SB
�$B
�$B
�$B
��B
�$B
�$B
�$B
�YB
�YB
��B
�$B
�YB
�YB
�$B
�YB
��B
�=B
�=B
��B
��B
��B
��B
��B
�B
��B
�~B
��B
�B
�B
�!B
��B
�VB
�!B
�'B
�\B
��B
�\B
�\B
�\B
�\B
��B
��B
��B
��B
��B
��B
�bB
�bB
�-B
��B
��B
��B
�bB
��B
�:B
�B
�nB
��B
��B
�zB
�LB
��B
�LB
��B
�RB
�RB
��B
�$B
��B
��B
��B
�_B
��B
��B
�*B
��B
�_B
��B
�6B
�6B
�6B
�B
�kB
�6B
�B
��B
�6B
�6B
�kB
��B
�=B
�B
�qB
��B
�B
�B
�IB
��B
�B
�OB
��B
��B
�!B
��B
�OB
��B
�'B
�'B
��B
�[B
�[B
�[B
�[B
��B
�-B
�aB
��B
��B
��B
��B
��B
��B
�9B
�9B
�B
�9B
�nB
�nB
��B
��B
�B
��B
�LB
��B
��B
�RB
��B
�XB
�XB
�$B
�XB
��B
��B
��B
��B
�dB
��B
��B
�B
��B
��B
�B
�B
�<B
�B
�<B
�B
��B
�qB
��B
��B
��B
��B
��B
��B
�<B
�<B
��B
��B
��B
��B
�BB
�B
�HB
��B
��B
�B
�OB
�OB
��B
��B
��B
� B
��B
�UB
��B
��B
�UB
�UB
�UB
�UB
�UB
�UB
��B
��B
��B
�UB
�UB
��B
��B
��B
��B
��B
��B
�'B
�[B
��B
�aB
ÖB
�3B
ĜB
ĜB
�3B
�3B
��B
�B
�mB
�9B
�mB
�9B
ŢB
��B
�?B
�tB
�?B
ƨB
��B
��B
��B
��B
��B
��B
�B
�EB
�EB
�EB
�EB
��B
��B
�zB
��B
��B
ɺB
ɆB
ɺB
��B
�#B
�#B
�#B
�#B
�XB
�XB
�XB
�XB
�XB
�XB
�#B
�XB
ʌB
��B
��B
��Bu�BtBu�Bu�BsMBu%Bu�Bt�Bu�Bu�Bs�Bt�Bu%BsBtTBv+Bu%BtBt�Bu�Bs�Bt�Bu�BtBt�Bu�BsBs�Bu%BsMBr�BtBt�Br�Bs�Bt�BsMBr|Bt�Bs�Br�Bs�Bt�Br�Bs�Bt�Bs�Br�Bt�Bt�Br�Bt�Bs�Br�Bt�BsMBr�Bt�Bs�BsBt�BtTBr�Bs�Bu%Bs�Br�Bt�Bt�BsBtTBu%BsMBtTBu�BsBs�BuZBt�BsBt�Bu�BsBtTBu�Bs�BsMBt�Bt�BsMBt�Bu�Bs�Bs�Bu%Bs�BsMBt�Bt�BsBs�Bu�Bs�BsMBu%Bt�Bs�BtTBu�Bs�BtBuZBt�BsBt�Bu�Bs�Bs�BuZBt�Bs�Bt�Bu�Bs�Bs�Bu�Bu%BsMBtBv+Bt�Bs�BuZBtTBs�Bt�Bu�Bs�BsMBt�Bu�BtBu�Bv+BtBtBv+Bu�Bs�Bu%Bv+Bt�Bt�Bv�Bt�Bt�Bu�Bv+Bs�Bv`Bu�BtTBu�Bv+Bu�BtTBt�Bv+Bu�BtTBtBu�BuZBs�Bt�Bu�Bu�BtBt�Bv+Bu�Bs�Bu%Bu�Bt�Bs�Bv+Bu�BrGBt�Bv+Bt�BsMBtTBv�Bt�Bs�BsBtBuZBtTBs�Bt�BuZBsBsMBt�Bu%Bs�BsMBu%Bt�Bs�BsBt�Bt�Bs�Br�Bt�Bt�Br�BtTBt�Bs�Br|Br|BtBsBq�Br�BsMBrBp�BqBs�Br�Bo�Bo5Bp;BqvB�MB��B�zB�zB��B{B'B*eB+kB8B;�B?HB?�BAUBDgBJXBV�B[#Bg�Bp�B�*B��B�_B� B�oB��B�uB��B�xB��B�B�=B�B�7B��B�B��B{�Bm)Bn�Bk�Bn�BsBlWBf�Bc BkBt�Bo5BpoBm)BqBs�Bv+B��B�%Bt�Bt�Bs�Bm�B`�B]�B`vBS&BUgBT�BR�BR�BMBNpBI�BL�BI�BO�B?�BAUBC-B>�B?BD�BI�B4�B6�B6�B4�B49B3�B4B1�B2�B2-B1�B1�B0!B0�B,B3�B5tB2�B<�B<jB@OBOvBI�BF�BQB[�B_Bk�Bc Ba�B`vBb�B^BYBXEBX�BZ�BQBC-B:^B;dB;�B=�BR�B;0B>�BE9BGBO�BQB_�Bi�BqBsMB|B�B��B��B��B��B��B��B�5B�yB�sB�B�EBרB�BуB��BуB��B�;B�&B�B�vB�pBߤB�B�B�lB��B�B�)B�;B��B�fB��B�rB�8B��B�DB��B�B�JB��B�%B�fB��B�B�%B�`B��B��B��B��B��B��B�B��B�%B�B�B�B�MB��B�B��B�JB��B�B	B
�B�B��BVB��BSB��B�BBیB�QBΥB��B��B��B�}B��B��B�dB��BɆB�vB̘B�2BɆB��B՛B�]B�B��B�B��B�B�B�B�"B��B�8B�|B�B�B�`B�B�5B�B�vB�B�dBߤB�KB��B��B� BбBѷBȴB��B�BרB�B�0B��BȀB�B��B��B��B��B�FB��B�	B�\B��BoiBx�Bs�BjKBdZBc�BcTBb�B`vB_pBd&BgmBa|B^�B]�B]�B`vB_�Bc�B^B]/BaB_�B^jB]/Bd�BffBffB[�BZ�BW�BW�BXyBU2BUgBT�BT�BV9B[#B]dBYBUgBQ�BNBV9Ba�BK�BM6BH�BI�BYBJ�BE�BFBH�BGEBH�BE�BE�BE�BC-BA�BC�BF?BHKBA�BGzB`�BHKBS�BCaBEmBB�BOBEBD�BEmBE9BF�BHKBMBN�BMBO�BQ�BR BT,BS�BR�BS�BT,BT�BT,BXyBZQBZ�BZ�BW?BX�B_BPBQ�BW�BU�BR�BC-BH�BHBF?BB[B@B<�B<�B>BB=�BB'B=<B7�B<6BDgB9$B@�BJXB@�B?HB?}BIRBJ#BMBJ�B@�BB'BB�BE9BD�BEmB@�BD3BD�BEmBC-BC�BD�BEmBD3BD�BE9BF?BEmBEBEmBEmBF�BEBC�BE9BFtBF�BEmBD3BEmBFBEBE9BEBEmBE�BD�BD�BD3BE�BD3BCaBA�BEBIBE�BE9BE�BDgBA BDgBAUBE�B��BQBL�BPBJ�BNBL0BN�BL�BK)BO�BQBI�BL�BIBHBN�BJ�BP�BL�BJ�BE�BT�BJ�BJ#BN�B^jBL0BPHBE�BD�BC�BTaBK)BM�Bb�BA BK�BQBL�BL�BJ�BK�BL�BO�BNBQ�BJ#BK�BJ�BU2BL�BVBK^BK�BMjBO�BOBB8RBN�BNBD�BNpBFtBJ�BH�BQBK�BK�BNBF�BR�BJ�BJ#BI�BG�BGEBIBJ�BMjBQ�BEmBPBO�Bc�BX�B�tB;�B6B6FB4�B9XB4�B8�B0�B33B1�B2-B6�B2aB2�B7B6B2�B33B0�B,qB+kB-CB-wB+kB,=B*0B*�B-B*eB+6B.IB&�B,�B-CB%�B)�B%�B(�B*�B(�B#B%�B6B$@B!-B�B�B�B~B!bB~BBVB�B7BBCBqBB�B{B+BYBSB�B�B(B�B	7B
	B�BYB
�B�cB��BB�B��B��B��B�,B��B�mB��B��BچB�pB�B�pB�yB�BΥB�BʌB��B�mB�BǮB��B�OB��B�dB҉BݘB��B�:B��B�B�SB�B��B��B�{B�B�oB��B�uB�oB�hB��B��B��B��B�~B��B�%B��B�oB��B�B�oB��B~�Bz�BzDG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202104292026552021042920265520210429202655202104292026552021042920265520210429202655SI  SI  ARFMARFM                                                                                                                                                2020111922012620201119220126IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022416401020210224164010QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022416401020210224164010QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021042910193920210429101939IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021042920270120210429202701IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021042920270120210429202701IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021042920270120210429202701IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                